import           Control.Monad (void, when)
import           Data.Default (def)
import           Network.MPD (withMPD, clear, status, stState)
import           Network.MPD.Commands.Extensions (toggle)
import           Options.Applicative
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitWith, exitSuccess, ExitCode(..))
import           System.IO (hPutStrLn, stderr, SeekMode(..))
import           System.Log.FastLogger ( newFileLoggerSet, newStdoutLoggerSet
                                       , defaultBufSize )
import           System.Posix.Daemon
import           System.Posix.Files (stdFileMode)
import           System.Posix.IO ( fdWrite, createFile, setLock
                                 , LockRequest(..) )
import           System.Posix.Process (getProcessID)

import Web.Radio
import qualified Web.Radio.Cmd as Cmd
import Web.Radio.Douban
import qualified Web.Radio.EightTracks as ET
import Web.Radio.Jing
import qualified Web.Radio.Reddit as Reddit

data Options = Options
    { optCommand    :: Command
    , optDaemon     :: Bool
    } deriving (Eq, Show)

data Command = CmdFM CmdSubCommand
             | DoubanFM DoubanSubCommand
             | EightTracks ETSubCommand
             | JingFM JingSubCommand
             | RedditFM RedditSubCommand
             | Status
             | Toggle
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

data ETSubCommand = ETListen String
                  | ETFeatured
                  | ETTrending
                  | ETNewest
                  | ETSearch String
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
                 <> command "8tracks"       (info (helper <*> etOptions)
                        (progDesc "8tracks.com commander"))
                 <> command "jing"          (info (helper <*> jingOptions)
                        (progDesc "jing.fm commander"))
                 <> command "reddit"        (info (helper <*> redditOptions)
                        (progDesc "radioreddit.com commander"))
                 <> command "status"        (info (pure Status)
                        (progDesc "Show current status"))
                 <> command "toggle"        (info (pure Toggle)
                        (progDesc "Toggles play/pause. Plays if stopped"))
                 <> command "kill"          (info (pure Kill)
                        (progDesc "Kill the current running lord session"))
                  )
    <*> switch (long "no-daemon" <> help "Don't detach from console")

main :: IO ()
main = do
    -- Make sure ~/.lord exists
    getLordDir >>= createDirectoryIfMissing False

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
        EightTracks subCommand ->
            case subCommand of
                ETListen mId -> etListen nodaemon mId
                ETFeatured   -> ET.featured >>= ET.pprMixes
                ETTrending   -> ET.trending >>= ET.pprMixes
                ETNewest     -> ET.newest   >>= ET.pprMixes
                ETSearch key -> etSearch key
        JingFM (JingListen key) -> jingListen nodaemon key
        RedditFM subCommand ->
            case subCommand of
                RedditListen genre   -> listen nodaemon (Reddit.Genre genre)
                RedditGenreList      -> Reddit.genres >>= Reddit.pprGenres
        Status -> lordStatus
        Toggle -> void $ withMPD toggle
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
        Success a -> return a
        Failure failure -> do
            progn <- getProgName
            let (msg, exit) = execFailure failure progn
            case exit of
                ExitSuccess -> putStrLn msg
                _           -> hPutStrLn stderr msg
            exitWith exit
        CompletionInvoked compl -> do
            progn <- getProgName
            msg <- execCompletion compl progn
            putStr msg
            exitSuccess

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
        (info (helper <*> (DoubanListen <$> argument str (
                  metavar "[<channel_id> | <album_url> | <muscian_url> | <musician_name>]")))
              (progDesc "Provide channel_id/album_url/musician_url/musician_name to listen to douban.fm"))
    <> command "search"
        (info (helper <*> (DoubanSearch <$> argument str (metavar "KEYWORDS")))
              (progDesc "Search channels"))
    <> command "hot" (info (pure DoubanHot) (progDesc "Hot channels"))
    <> command "trending"
        (info (pure DoubanTrending) (progDesc "Trending up channels"))
    )

etOptions :: Parser Command
etOptions = EightTracks <$> subparser
    ( command "listen"
        (info (helper <*> (ETListen <$> argument str (metavar "[<mix_id> | <mix_url>]")))
              (progDesc "Provide mix_id/mix_url to listen to 8tracks.com"))
    <> command "featured" (info (pure ETFeatured) (progDesc "Featured mixes"))
    <> command "trending" (info (pure ETFeatured) (progDesc "Trending mixes"))
    <> command "newest" (info (pure ETFeatured) (progDesc "Newest mixes"))
    <> command "search"
        (info (helper <*> (ETSearch <$> argument str (metavar "KEYWORDS")))
              (progDesc "Search mixes"))
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

etListen :: Bool -> Keywords -> IO ()
etListen nodaemon k = do
    mId <- ET.getMixId k
    tok <- readToken ET.eight $ show mId
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ ET.userName tok'
            listen nodaemon tok'
        _         -> do
            param <- login k :: IO (Param ET.EightTracks)
            listen nodaemon param

etSearch :: String -> IO ()
etSearch key = ET.search key >>= ET.pprMixes

jingListen :: Bool -> Keywords -> IO ()
jingListen nodaemon k = do
    tok <- readToken jing k
    case tok of
        Just tok' -> do
            putStrLn $ "Welcome back, " ++ nick tok'
            listen nodaemon tok'
        _         -> do
            param <- login k :: IO (Param Jing)
            listen nodaemon param

listen :: Radio a => Bool -> Param a -> IO ()
listen nodaemon param = do
    -- mplayer backend won't work in daemon mode!
    st <- withMPD status
    nodaemon' <- case st of
                     Right _ -> return nodaemon
                     Left  e -> print e >>
                                putStrLn "Lord will run in foreground" >>
                                return True

    pid <- getPidFile
    logger <- if nodaemon' then newStdoutLoggerSet defaultBufSize
                           else do
                  fp <- getLogFile
                  newFileLoggerSet defaultBufSize fp
    let listen' = play logger param []
    running <- isRunning pid
    when running $ killAndWait pid
    if nodaemon' then runInForeground pid listen'
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
