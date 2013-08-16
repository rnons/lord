import Control.Monad (when)
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Data.Default (def)
import Network.MPD (withMPD, clear)
import Options.Applicative
import System.Posix.Daemon

import Radio
import Radio.Douban
import Radio.Jing

data Options = Options
    { optCommand    :: Command
    , optDaemon     :: Bool
    } deriving (Eq, Show)

data Command = DoubanRadio DoubanSubCommand
             | JingRadio JingSubCommand
             | Kill
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
    <$> subparser ( command "douban"        (info (helper <*> doubanOptions)
                        (progDesc "douban.fm commander"))
                 <> command "jing"          (info (helper <*> jingOptions)
                        (progDesc "jing.fm commander"))
                 <> command "kill"          (info (pure Kill)
                        (progDesc "kill the current running lord session"))
                  )
    <*> switch (long "no-daemon" <> help "don't detach from console")

main :: IO ()
main = do
    pid <- getPid
    o <- execParser $ info (helper <*> optParser) 
                           (fullDesc <> header "Lord: radio commander")
    case optCommand o of
        DoubanRadio subCommand -> 
            case subCommand of
                 DoubanListen param -> listen (optDaemon o) pid doubanListen param
                 DoubanHot -> doubanHot
                 DoubanTrending -> doubanTrending
                 DoubanSearch param -> doubanSearch param
        JingRadio (JingListen param) -> jingListen (optDaemon o) pid param
        Kill -> killLord

doubanOptions :: Parser Command
doubanOptions = DoubanRadio <$> 
                   subparser ( command "listen" (info doubanListenOptions
                                    (progDesc "Provide cid/musician to listen to douban.fm"))
                            <> command "search" (info doubanSearchOptions
                                    (progDesc "search channels"))
                            <> command "hot" (info (pure DoubanHot)
                                    (progDesc "hot channels"))
                            <> command "trending" (info (pure DoubanTrending)
                                    (progDesc "trending up channels"))
                             )

doubanListenOptions :: Parser DoubanSubCommand
doubanListenOptions = 
    helper <*> 
    ( DoubanListen <$> argument str (metavar "[<channel_id> | <musician>]") )

doubanSearchOptions :: Parser DoubanSubCommand
doubanSearchOptions = 
    helper <*> 
    ( DoubanSearch <$> argument str (metavar "KEYWORDS") )

jingOptions :: Parser Command
jingOptions = JingRadio <$> 
                 subparser ( command "listen" (info jingListenOptions
                                 (progDesc "Provide keywords to listen to jing.fm"))
                           )

jingListenOptions :: Parser JingSubCommand
jingListenOptions = 
    helper <*> 
    ( JingListen <$> argument str (metavar "KEYWORDS") )

doubanListen :: String -> IO ()
doubanListen p 
    | isChId p = play (Cid $ read p) []
    | otherwise = play (Musician p) []
  where
    isChId = and . fmap isDigit

doubanHot :: IO ()
doubanHot = hot >>= pprChannels

doubanTrending :: IO ()
doubanTrending = trending >>= pprChannels

doubanSearch :: String -> IO ()
doubanSearch key = search key >>= pprChannels

jingListen :: Bool -> FilePath -> String -> IO ()
jingListen nodaemon pid p = do
    tok <- readToken p
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ C.unpack (nick tok')
            play' tok'
        _         -> do
            mtok <- login p
            play' mtok
  where
    play' tok =
        if nodaemon then play tok []
                    else do
                        running <- isRunning pid
                        when running $ killAndWait pid 
                        runDetached (Just pid) def (play tok [])

listen :: Bool -> FilePath -> (String -> IO ()) -> String -> IO ()
listen nodaemon pid f p =
    if nodaemon then f p
                else do
                    running <- isRunning pid
                    when running $ killAndWait pid 
                    runDetached (Just pid) def (f p)

killLord :: IO ()
killLord = withMPD clear >> getPid >>= kill

getPid :: IO FilePath
getPid = do
    home <- getRadioDir
    return $ home ++ "/lord.pid"
