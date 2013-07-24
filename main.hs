import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Options.Applicative

import Radio
import Radio.Douban
import Radio.Jing

data Options = Options
    { optCommand    :: Command
    } deriving (Eq, Show)

data Command = DoubanRadio DoubanSubCommand
             | JingRadio JingSubCommand
    deriving (Eq, Show)

data DoubanSubCommand = DoubanListen String
                      | DoubanHot
                      | DoubanTrending
                      | DoubanSearch String
    deriving (Eq, Show)

data JingSubCommand = JingListen String
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
        DoubanRadio subCommand -> 
            case subCommand of
                 DoubanListen param -> doubanListen param
                 DoubanHot -> doubanHot
                 DoubanTrending -> doubanTrending
                 DoubanSearch param -> doubanSearch param
        JingRadio (JingListen param) -> jingListen param

doubanOptions :: Parser Command
doubanOptions = DoubanRadio <$> 
                   subparser ( command "listen" (info doubanListenOptions
                                    (progDesc "listen to cid/musician"))
                            <> command "hot" (info (pure DoubanHot)
                                    (progDesc "hot channels"))
                            <> command "trending" (info (pure DoubanTrending)
                                    (progDesc "trending up channels"))
                            <> command "search" (info doubanSearchOptions
                                    (progDesc "search channels"))
                             )

doubanListenOptions :: Parser DoubanSubCommand
doubanListenOptions = DoubanListen <$> argument str ( metavar "[<channel_id> | <musician>]" )

doubanSearchOptions :: Parser DoubanSubCommand
doubanSearchOptions = DoubanSearch <$> argument str ( metavar "KEYWORDS" )

jingOptions :: Parser Command
jingOptions = JingRadio <$> 
                 subparser ( command "listen" (info jingListenOptions
                                 (progDesc "listen to jing.fm"))
                           )

jingListenOptions :: Parser JingSubCommand
jingListenOptions = JingListen <$> argument str ( metavar "KEYWORDS" )

doubanListen :: String -> IO ()
doubanListen p 
    | isChId p = play (Cid $ read p) []
    | otherwise = play (Musician p) []
  where
    isChId = and . fmap isDigit

doubanHot = hot >>= pprChannels

doubanTrending = trending >>= pprChannels

doubanSearch key = search key >>= pprChannels

jingListen :: String -> IO ()
jingListen p = do
    tok <- readToken p
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ C.unpack (nick tok')
            play tok' []
        _         -> do
            mtok <- login p
            play mtok []

