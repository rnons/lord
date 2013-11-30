{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A generic interface to online radio services
module Radio
  ( SongMeta(..)
  , Radio(..)
  , NeedLogin(..)
  , getLordDir
  , getPidFile
  , getLogFile
  , getStateFile
  , writeLog
  ) where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (liftM, when, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Conduit (runResourceT, ($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Data.Yaml
import           Network.HTTP.Conduit hiding (path)
import           Network.MPD hiding (play, Value)
import qualified Network.MPD as MPD
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.FastLogger
import           System.Process

eof :: MVar ()
eof = unsafePerformIO newEmptyMVar

data MpState = MpState
    { readh     :: MVar Handle
    , writeh    :: MVar Handle
    , mpHdl     :: MVar ProcessHandle
    , ended     :: MVar ()
    }

emptySt :: MpState
emptySt = MpState
    { readh     = unsafePerformIO newEmptyMVar
    , writeh    = unsafePerformIO newEmptyMVar
    , mpHdl     = unsafePerformIO newEmptyMVar
    , ended     = unsafePerformIO newEmptyMVar
    }

mplayerState :: MVar MpState
mplayerState = unsafePerformIO $ newMVar emptySt

getsST :: (MpState -> a) -> IO a
getsST f = withST (return . f)

-- | Perform a (read-only) IO action on the state
withST :: (MpState -> IO a) -> IO a
withST f = readMVar mplayerState >>= f

-- | Modify the state with a pure function
silentlyModifyST :: (MpState -> MpState) -> IO ()
silentlyModifyST  f = modifyMVar_ mplayerState (return . f)

data SongMeta = SongMeta 
    { artist    :: String
    , album     :: String
    , title     :: String
    } 

instance Show SongMeta where
    show meta = artist meta ++ " - " ++ title meta 

class FromJSON a => Radio a where
    data Param a :: *

    parsePlaylist :: Value -> [a]

    getPlaylist :: Param a -> IO [a]

    songUrl :: Param a -> a -> IO String

    songMeta :: a -> SongMeta

    tagged :: a -> Bool

    -- Mpd can play remote mp3 files directly.
    -- For m4a files, lord download it as ~/.lord/lord.m4a
    playable :: a -> Bool
    playable _ = True

    reportRequired :: a -> Bool
    reportRequired _ = False

    report :: Param a -> a -> IO ()
    report _ _ = return ()

    reportLoop :: Param a -> a -> IO ()
    reportLoop param x = do
        time <- liftM stTime <$> withMPD status
        case time of
            Right (elapsed, _) ->
                if elapsed < 30 
                    then threadDelay (5*1000000) >> reportLoop param x
                    else report param x
            Left err -> print err

    play :: Logger -> Param a -> [a] -> IO ()
    play logger reqData [] = getPlaylist reqData >>= play logger reqData
    play logger reqData (x:xs) = do
        st <- withMPD status
        case st of
            Right _ -> playWithMPD logger reqData (x:xs)
            Left  _ -> do
                -- initialize mplayer as early as possible
                forkIO mplayerInit
                forkIO mplayerWait
                playWithMplayer logger reqData (x:xs)

playWithMPD :: Radio a => Logger -> Param a -> [a] -> IO ()
playWithMPD logger reqData [] = 
    getPlaylist reqData >>= playWithMPD logger reqData
playWithMPD logger reqData (x:xs)
    | playable x = loadAndPlay logger reqData (x:xs)
    | otherwise = downloadAndPlay logger reqData (x:xs)

loadAndPlay :: Radio a => Logger -> Param a -> [a] -> IO ()
loadAndPlay logger reqData [] = 
    getPlaylist reqData >>= loadAndPlay logger reqData
loadAndPlay logger reqData (x:xs) = do
    surl <- songUrl reqData x
    print surl
    when (surl /= "") $ do
        logAndReport logger reqData x
        mpdLoad $ Path $ C.pack surl
        takeMVar eof                     -- Finished
    loadAndPlay logger reqData xs
  where
    mpdLoad :: Path -> IO ()
    mpdLoad path = do
        withMPD $ do
            clear
            add path
        withMPD $ MPD.play Nothing
        mpdPlay

    mpdPlay :: IO ()
    mpdPlay = do
        st <- mpdState
        if st == Right Stopped 
            then putMVar eof ()
            else mpdPlay

downloadAndPlay :: Radio a => Logger -> Param a -> [a] -> IO ()
downloadAndPlay logger reqData [] = 
    getPlaylist reqData >>= downloadAndPlay logger reqData
downloadAndPlay logger reqData (x:xs) = do
    surl <- songUrl reqData x
    print surl
    req <- parseUrl surl
    home <- getLordDir
    manager <- newManager def
    forkIO $ E.catch 
        (do
            logAndReport logger reqData x

            runResourceT $ do 
                res <- http req manager
                responseBody res $$+- sinkFile (home ++ "/lord.m4a")
            -- This will block until eof.

            putMVar eof ())
        (\e -> do
            print (e :: E.SomeException)
            writeLog logger $ show e
            downloadAndPlay logger reqData xs
            )
    threadDelay (3*1000000)
    mpdLoad m4a
    downloadAndPlay logger reqData xs
  where
    m4a = "lord/lord.m4a"

    mpdLoad :: Path -> IO ()
    mpdLoad path = do
        s <- withMPD $ do
                clear
                update [Path "lord"]
                add path
        case s of
            Right _ -> do
                withMPD $ MPD.play Nothing
                mpdPlay
            _                  -> mpdLoad path

    mpdPlay :: IO ()
    mpdPlay = do
        st <- mpdState
        bd <- isEmptyMVar eof
        if st == Right Stopped 
            then if bd 
                    then do                              -- Slow Network
                        withMPD $ MPD.play Nothing
                        mpdPlay
                    else do
                        withMPD clear
                        takeMVar eof                     -- Finished
            else mpdPlay                                 -- Pause

playWithMplayer :: Radio a => Logger -> Param a -> [a] -> IO ()
playWithMplayer logger reqData [] = 
    getPlaylist reqData >>= playWithMplayer logger reqData
playWithMplayer logger reqData (x:xs) = do
    surl <- songUrl reqData x
    when (surl /= "") $ do
        logAndReport logger reqData x
        let input = "loadfile " ++ surl
        mplayerSend input
        e <- getsST ended
        takeMVar e      -- block until current song finished playing
    playWithMplayer logger reqData xs

-- mplayer slave mode protocol:
-- http://www.mplayerhq.hu/DOCS/tech/slave.txt
mplayerInit :: IO ()
mplayerInit = do
    let sh = "mplayer -msglevel global=6:statusline=6 -slave -idle -really-quiet -cache 2048 -cache-min 5 -novideo"
    (Just hin, Just hout, Just herr, hdl) <-
        createProcess (shell sh){ std_in = CreatePipe
                                , std_out = CreatePipe
                                , std_err = CreatePipe }
    mhin <- newMVar hin
    mhout <- newMVar hout
    mhdl <- newMVar hdl
    silentlyModifyST $ \st -> st { writeh = mhin
                                 , readh = mhout
                                 , mpHdl = mhdl }
    waitForProcess hdl
    hClose herr

-- | If song endded or skipped, putMVar.
mplayerWait :: IO ()
mplayerWait = do
    hout <- getsST readh >>= readMVar
    line <- hGetLine hout           -- consume hout to prevent overflow
    case line of
         "EOF code: 1  " -> do      -- song end
             e <- getsST ended
             putMVar e ()
         "EOF code: 4  " -> do      -- next song
             e <- getsST ended
             putMVar e ()
         _ -> return ()
    mplayerWait

mplayerSend :: String -> IO ()
mplayerSend msg = withST $ \st -> do
    hin <- readMVar (writeh st)
    hPutStrLn hin msg
    hFlush hin

logAndReport :: Radio a => Logger -> Param a -> a -> IO ()
logAndReport logger reqData x = do
    writeLog logger (show $ songMeta x) 
    getStateFile >>= flip writeFile (show $ songMeta x)
    -- Report song played if needed
    when (reportRequired x) $ void (forkIO $ reportLoop reqData x)

mpdState :: IO (MPD.Response State)
mpdState = do
    withMPD $ idle [PlayerS]   
    -- This will block until paused/finished.

    st <- liftM stState <$> withMPD status
    print st
    return st

class (Radio a, ToJSON (Param a), ToJSON (Config a)) => NeedLogin a where
    login :: String -> IO (Param a)
    login keywords = do
        hSetBuffering stdout NoBuffering
        hSetEcho stdin True 
        putStrLn "Please Log in"
        putStr "Email: "
        email <- getLine
        putStr "Password: "
        hSetEcho stdin False 
        pwd <- getLine
        hSetEcho stdin True 
        putStrLn ""
        mtoken <- createSession keywords email pwd
        case mtoken of
             Just tok -> do
                 saveToken tok
                 return tok
             Nothing  -> do
                 putStrLn "ERROR: Invalid email or password!"
                 login keywords

    createSession :: String -> String -> String -> IO (Maybe (Param a))

    data Config a :: *

    mkConfig :: Param a -> Config a

    saveToken :: Param a -> IO ()
    saveToken tok = do
        home <- getLordDir
        let yml = home ++ "/lord.yml"
        exist <- doesFileExist yml
        bs <- if exist
            then B.readFile yml
            else return ""
        let config = mkConfig tok
        B.writeFile yml $ B.append bs (encode config)
        putStrLn $ "Your token has been saved to " ++ yml

    readToken :: String -> IO (Maybe (Param a))

getLordDir :: IO FilePath
getLordDir = (++ "/.lord") <$> getHomeDirectory

getPidFile :: IO FilePath
getPidFile = (++ "/lord.pid") <$> getLordDir

getLogFile :: IO FilePath
getLogFile = (++ "/lord.log") <$> getLordDir

getStateFile :: IO FilePath
getStateFile = (++ "/lordstate") <$> getLordDir

formatLogMessage :: IO ZonedDate -> String -> IO [LogStr]
formatLogMessage getdate msg = do
    now <- getdate
    return 
        [ LB now
        , LB " : "
        , LS $ encodeString msg
        , LB "\n"
        ]

writeLog :: Logger -> String -> IO ()
writeLog l msg = formatLogMessage (loggerDate l) msg >>= loggerPutStr l
