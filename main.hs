import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isDigit)
import           Data.Default (def)
import           Network.MPD (withMPD, clear, status, stState)
import           Options.Applicative
import           System.Directory (createDirectoryIfMissing)
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

type Keywords = String

optParser :: Parser Options
optParser = Options 
    <$> subparser ( command "cmd"           (info (helper <*> cmdOptions)
                        (progDesc "cmd.fm commander"))
                 <> command "douban"        (info (helper <*> doubanOptions)
                        (progDesc "douban.fm commander"))
                 <> command "jing"          (info (helper <*> jingOptions)
                        (progDesc "jing.fm commander"))
                 <> command "status"        (info (pure Status)
                        (progDesc "show current status"))
                 <> command "kill"          (info (pure Kill)
                        (progDesc "kill the current running lord session"))
                  )
    <*> switch (long "no-daemon" <> help "don't detach from console")

main :: IO ()
main = do
    o <- execParser $ info (helper <*> optParser) 
                           (fullDesc <> header "Lord: radio commander")
    let nodaemon = optDaemon o
    case optCommand o of
        CmdFM subCommand ->
            case subCommand of
                CmdListen genre   -> listen nodaemon (cmd genre)
                CmdGenreList      -> cmdGenres
        DoubanFM subCommand -> 
            case subCommand of
                 DoubanListen key -> listen nodaemon (douban key)
                 DoubanHot        -> doubanHot
                 DoubanTrending   -> doubanTrending
                 DoubanSearch key -> doubanSearch key
        JingFM (JingListen key) -> jingListen nodaemon key
        Status -> lordStatus
        Kill -> killLord

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

cmdGenres :: IO ()
cmdGenres = genres >>= pprGenres

doubanHot :: IO ()
doubanHot = hot >>= pprChannels

doubanTrending :: IO ()
doubanTrending = trending >>= pprChannels

doubanSearch :: String -> IO ()
doubanSearch key = search key >>= pprChannels

cmd :: Keywords -> Radio.Param Cmd
cmd = Genre

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
listen nodaemon param= do
    -- Make sure ~/.lord exists
    getLordDir >>= createDirectoryIfMissing False

    pid <- getPid
    logger <- if nodaemon then mkLogger True stdout 
              else getLogFile >>= flip openFile AppendMode >>= mkLogger True
    let listen = play logger param []
    if nodaemon then listen
                else do
                    running <- isRunning pid
                    when running $ killAndWait pid 
                    runDetached (Just pid) def listen

killLord :: IO ()
killLord = withMPD clear >> getPid >>= kill

lordStatus :: IO ()
lordStatus = do
    st <- fmap stState <$> withMPD status
    let state = case st of
            Right s  -> show s
            Left err -> error $ show err
    song <- getStateFile >>= readFile 
    putStrLn $ "[" ++ state ++ "] " ++ song
