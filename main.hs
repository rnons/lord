import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isDigit)
import           Data.Default (def)
import           Network.MPD (withMPD, clear, status, stState)
import           Options.Applicative
import           Options.Applicative.Types (ParserPrefs)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitWith, exitSuccess, ExitCode(..))
import           System.IO ( openFile, IOMode(AppendMode)
                           , hPutStr, stdout, stderr, SeekMode(..) )
import           System.Log.FastLogger (mkLogger)
import           System.Posix.Daemon
import           System.Posix.Files (stdFileMode)
import           System.Posix.IO ( fdWrite, createFile, setLock
                                 , LockRequest(..) )
import           System.Posix.Process (getProcessID)

import Radio
import qualified Radio.Cmd as Cmd
import Radio.Douban
import Radio.Jing 
import qualified Radio.Reddit as Reddit

data Options = Options
    { optCommand    :: Command
    , optDaemon     :: Bool
    } deriving (Eq, Show)

data Command = CmdFM CmdSubCommand
             | DoubanFM DoubanSubCommand
             | JingFM JingSubCommand
             | RedditFM RedditSubCommand
             | Status
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

data RedditSubCommand = RedditListen String
                      | RedditGenreList
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
                 <> command "reddit"        (info (helper <*> redditOptions)
                        (progDesc "radioreddit.com commander"))
                 <> command "status"        (info (pure Status)
                        (progDesc "show current status"))
                 <> command "kill"          (info (pure Kill)
                        (progDesc "kill the current running lord session"))
                  )
    <*> switch (long "no-daemon" <> help "don't detach from console")

main :: IO ()
main = do
    o <- execParser' $ info (helper <*> optParser) 
                           (fullDesc <> header "Lord: radio commander")
    let nodaemon = optDaemon o
    case optCommand o of
        CmdFM subCommand ->
            case subCommand of
                CmdListen genre   -> listen nodaemon (Cmd.Genre genre)
                CmdGenreList      -> Cmd.genres >>= Cmd.pprGenres
        DoubanFM subCommand -> 
            case subCommand of
                 DoubanListen key -> listen nodaemon (douban key)
                 DoubanHot        -> doubanHot
                 DoubanTrending   -> doubanTrending
                 DoubanSearch key -> doubanSearch key
        JingFM (JingListen key) -> jingListen nodaemon key
        RedditFM subCommand ->
            case subCommand of
                RedditListen genre   -> listen nodaemon (Reddit.Genre genre)
                RedditGenreList      -> Reddit.genres >>= Reddit.pprGenres
        Status -> lordStatus
        Kill -> killLord

-- Taken from Options.Applicative.Extra
execParser' :: ParserInfo a -> IO a
execParser' = customExecParser' (prefs idm)

-- Taken from Options.Applicative.Extra
customExecParser' :: ParserPrefs -> ParserInfo a -> IO a
customExecParser' pprefs pinfo = do
    args <- getArgs
    
    -- My modification!
    -- Run lord with no args is equivalent to run lord status.
    when (null args) $ lordStatus >> exitSuccess
  
    case execParserPure pprefs pinfo args of
        Right a -> return a
        Left failure -> do
            progn <- getProgName
            let c = errExitCode failure
            msg <- errMessage failure progn
            case c of
                ExitSuccess -> putStr msg
                _           -> hPutStr stderr msg
            exitWith c

cmdOptions :: Parser Command
cmdOptions = CmdFM <$> subparser
    ( command "listen" 
        (info (helper <*> (CmdListen <$> argument str (metavar "GENRE")))
              (progDesc "Provide genre to listen to cmd.fm"))
    <> command "genres" 
        (info (pure CmdGenreList) (progDesc "List available genres"))
    )

doubanOptions :: Parser Command
doubanOptions = DoubanFM <$> subparser 
    ( command "listen" 
        (info (helper <*> (DoubanListen <$> argument str (metavar "[<channel_id> | <musician>]")))
              (progDesc "Provide cid/musician to listen to douban.fm"))
    <> command "search" 
        (info (helper <*> (DoubanSearch <$> argument str (metavar "KEYWORDS")))
              (progDesc "search channels"))
    <> command "hot" (info (pure DoubanHot) (progDesc "hot channels"))
    <> command "trending" 
        (info (pure DoubanTrending) (progDesc "trending up channels"))
    )

jingOptions :: Parser Command
jingOptions = JingFM <$> subparser 
    ( command "listen" 
        (info (helper <*> (JingListen <$> argument str (metavar "KEYWORDS")))
              (progDesc "Provide keywords to listen to jing.fm"))
    )

redditOptions :: Parser Command
redditOptions = RedditFM <$> subparser
    ( command "listen" 
        (info (helper <*> (RedditListen <$> argument str (metavar "GENRE")))
              (progDesc "Provide genre to listen to radioreddit.com"))
    <> command "genres" 
        (info (pure RedditGenreList) (progDesc "List available genres"))
    )

doubanHot :: IO ()
doubanHot = hot >>= pprChannels

doubanTrending :: IO ()
doubanTrending = trending >>= pprChannels

doubanSearch :: String -> IO ()
doubanSearch key = search key >>= pprChannels

douban :: Keywords -> Radio.Param Douban
douban k
    | isChId k = Cid $ read k
    | otherwise = Musician k
  where
    isChId = and . fmap isDigit

jingListen :: Bool -> Keywords -> IO ()
jingListen nodaemon k = do
    tok <- readToken k
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ C.unpack (nick tok')
            listen nodaemon tok'
        _         -> login k >>= listen nodaemon

listen :: Radio a => Bool -> Radio.Param a -> IO ()
listen nodaemon param = do
    -- Make sure ~/.lord exists
    getLordDir >>= createDirectoryIfMissing False

    pid <- getPidFile
    logger <- if nodaemon then mkLogger True stdout 
              else getLogFile >>= flip openFile AppendMode >>= mkLogger True
    let listen' = play logger param []
    running <- isRunning pid
    when running $ killAndWait pid 
    if nodaemon then runInForeground pid listen'
                else runDetached (Just pid) def listen'

-- Partially taken from System.Posix.Daemon module
runInForeground :: FilePath -> IO () -> IO ()
runInForeground pidFile program = do
    fd <- createFile pidFile stdFileMode
    setLock fd (WriteLock, AbsoluteSeek, 0, 0)
    pid <- getProcessID
    fdWrite fd (show pid)
    program

killLord :: IO ()
killLord = withMPD clear >> getPidFile >>= kill

lordStatus :: IO ()
lordStatus = do
    running <- getPidFile >>= isRunning
    myStatus <- 
        if running then do
            st <- fmap stState <$> withMPD status
            let state = case st of
                    Right s  -> show s
                    Left err -> error $ show err
            song <- getStateFile >>= readFile 
            return $ "[" ++ state ++ "] " ++ song
        else return "Not running!"
    putStrLn myStatus
