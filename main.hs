import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Options.Applicative

import Radio
import Radio.Douban
import Radio.Jing

data Options = Options
    { optCommand    :: Command
    } deriving (Eq, Show)

data Command = DoubanRadio String
             | JingRadio String
    deriving (Eq, Show)


optParser :: Parser Options
optParser = Options 
    <$> subparser ( command "douban"       (info doubanOptions
                        (progDesc "Play douban.fm"))
                 <> command "jing"         (info jingOptions
                        (progDesc "Play jing.fm"))
                  )

main :: IO ()
main = do
    o <- execParser $ info (helper <*> optParser) 
                           (fullDesc <> header "Lord: radio commander")
    case optCommand o of
        DoubanRadio param -> douban param
        JingRadio param   -> jing param

doubanOptions :: Parser Command
doubanOptions = DoubanRadio <$> argument str ( metavar "[<channel_id> | <musician>]" )

jingOptions :: Parser Command
jingOptions = JingRadio <$> argument str ( metavar "<keywords>" )

douban :: String -> IO ()
douban p 
    | isChId p = play (Cid $ read p) []
    | otherwise = play (Musician p) []
  where
    isChId = and . fmap isDigit

jing :: String -> IO ()
jing p = do
    tok <- readToken p
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ C.unpack (nick tok')
            play tok' []
        _         -> do
            mtok <- login p
            play mtok []

