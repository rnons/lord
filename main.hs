import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isDigit)
import           Data.Default (def)
import           Network.MPD (withMPD, clear)
import           Options.Applicative
import           System.IO (openFile, IOMode(AppendMode), stdout)
import           System.Log.FastLogger (mkLogger, Logger)
import           System.Posix.Daemon

import Radio
import Radio.Cmd
import Radio.Douban
import Radio.Jing

data Options = Options
    { optCommand    :: Command
    , optDaemon     :: Bool
    } deriving (Eq, Show)

data Command = CmdFM CmdSubCommand
             | DoubanFM DoubanSubCommand
             | JingFM JingSubCommand
             | Kill
    deriving (Eq, Show)

data CmdSubCommand = CmdListen String
                   | CmdGenreList
    deriving (Eq, Show)

data DoubanSubCommand = DoubanListen String
                      | DoubanHot
                      | DoubanTrending
                      | DoubanSearch String
    deriving (Eq, Show)

data JingSubCommand = JingListen String
    deriving (Eq, Show)

type Keywords = String

optParser :: Parser Options
optParser = Options 
    <$> subparser ( command "cmd"           (info (helper <*> cmdOptions)
                        (progDesc "cmd.fm commander"))
                 <> command "douban"        (info (helper <*> doubanOptions)
                        (progDesc "douban.fm commander"))
                 <> command "jing"          (info (helper <*> jingOptions)
                        (progDesc "jing.fm commander"))
                 <> command "kill"          (info (pure Kill)
                        (progDesc "kill the current running lord session"))
                  )
    <*> switch (long "no-daemon" <> help "don't detach from console")

main :: IO ()
main = do
    o <- execParser $ info (helper <*> optParser) 
                           (fullDesc <> header "Lord: radio commander")
    case optCommand o of
        CmdFM subCommand ->
            case subCommand of
                CmdListen genre   -> listen (cmdListen genre) (optDaemon o)
                CmdGenreList      -> cmdGenres
        DoubanFM subCommand -> 
            case subCommand of
                 DoubanListen key -> listen (doubanListen key) (optDaemon o)
                 DoubanHot        -> doubanHot
                 DoubanTrending   -> doubanTrending
                 DoubanSearch key -> doubanSearch key
        JingFM (JingListen key) -> jingListen key (optDaemon o)
        Kill -> killLord

cmdOptions :: Parser Command
cmdOptions = CmdFM <$> subparser
    ( command "listen" (info cmdListenOptions
        (progDesc "Provide genre to listen to cmd.fm"))
    <> command "genres" (info (pure CmdGenreList)
        (progDesc "List available genres"))
    )

cmdListenOptions :: Parser CmdSubCommand
cmdListenOptions = helper <*>
    ( CmdListen <$> argument str (metavar "GENRE") )

doubanOptions :: Parser Command
doubanOptions = DoubanFM <$> subparser 
    ( command "listen" (info doubanListenOptions
        (progDesc "Provide cid/musician to listen to douban.fm"))
    <> command "search" (info doubanSearchOptions
        (progDesc "search channels"))
    <> command "hot" (info (pure DoubanHot)
        (progDesc "hot channels"))
    <> command "trending" (info (pure DoubanTrending)
        (progDesc "trending up channels"))
    )

doubanListenOptions :: Parser DoubanSubCommand
doubanListenOptions = helper <*> 
    ( DoubanListen <$> argument str (metavar "[<channel_id> | <musician>]") )

doubanSearchOptions :: Parser DoubanSubCommand
doubanSearchOptions = helper <*> 
    ( DoubanSearch <$> argument str (metavar "KEYWORDS") )

jingOptions :: Parser Command
jingOptions = JingFM <$> subparser 
    ( command "listen" (info jingListenOptions
        (progDesc "Provide keywords to listen to jing.fm"))
    )

jingListenOptions :: Parser JingSubCommand
jingListenOptions = helper <*> 
    ( JingListen <$> argument str (metavar "KEYWORDS") )

cmdListen :: Keywords -> Radio.Param Cmd
cmdListen = Genre

doubanListen :: Keywords -> Radio.Param Douban
doubanListen k
    | isChId k = Cid $ read k
    | otherwise = Musician k
  where
    isChId = and . fmap isDigit

listen :: Radio a => Radio.Param a -> Bool -> IO ()
listen param nodaemon = do
    pid <- getPid
    logger <- if nodaemon then mkLogger True stdout 
              else getLogFile >>= flip openFile AppendMode >>= mkLogger True
    let listen = play logger param []
    if nodaemon then listen
                else do
                    running <- isRunning pid
                    when running $ killAndWait pid 
                    runDetached (Just pid) def listen

cmdGenres :: IO ()
cmdGenres = genres >>= pprGenres

doubanHot :: IO ()
doubanHot = hot >>= pprChannels

doubanTrending :: IO ()
doubanTrending = trending >>= pprChannels

doubanSearch :: String -> IO ()
doubanSearch key = search key >>= pprChannels

jingListen :: Keywords -> Bool -> IO ()
jingListen k nodaemon = do
    pid <- getPid
    logger <- if nodaemon then mkLogger True stdout 
              else getLogFile >>= flip openFile AppendMode >>= mkLogger True
    let play' tok =
            if nodaemon then play logger tok []
            else do
                running <- isRunning pid
                when running $ killAndWait pid 
                runDetached (Just pid) def (play logger tok [])
    tok <- readToken k
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ C.unpack (nick tok')
            play' tok'
        _         -> do
            mtok <- login k
            play' mtok

killLord :: IO ()
killLord = withMPD clear >> getPid >>= kill

getPid :: IO FilePath
getPid = do
    home <- getRadioDir
    return $ home ++ "/lord.pid"

getLogFile :: IO FilePath
getLogFile = do
    home <- getRadioDir
    return $ home ++ "/lord.log"
