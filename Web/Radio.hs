{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A generic interface to online radio services
module Web.Radio
  ( SongMeta(..)
  , Radio(..)
  , NeedLogin(..)
  , getLordDir
  , getPidFile
  , getLogFile
  , getStateFile
  , writeLog
  ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad (liftM, when, void)
import           Data.Aeson hiding (encode)
import qualified Data.ByteString as B
import           Data.Maybe (isJust, fromJust)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Yaml
import           Network.MPD hiding (play, pause, config, Value)
import qualified Network.MPD as MPD
import           Network.MPD.Core (getResponse)
import           Network.Wai.Logger (ZonedDate, clockDateCacher)
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.FastLogger
import           System.Process


eof :: MVar ()
eof = unsafePerformIO newEmptyMVar

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

    reportRequired :: a -> Bool
    reportRequired _ = False

    report :: Param a -> a -> IO ()
    report _ _ = return ()

    reportLoop :: Param a -> a -> IO ()
    reportLoop param x = do
        time <- liftM stTime <$> withMPD status
        case time of
            Right (Just (elapsed, _)) ->
                if elapsed < 30
                    then threadDelay (5*1000000) >> reportLoop param x
                    else report param x
            Right Nothing -> return ()
            Left err -> print err

    play :: LoggerSet -> Param a -> [a] -> IO ()
    play logger reqData xxs = do
        st <- withMPD status
        case st of
            Right _ -> playWithMPD logger reqData xxs
            Left  _ -> playWithMplayer logger reqData xxs

-- MPD can play remote m4a files directly since version-0.18
playWithMPD :: Radio a => LoggerSet -> Param a -> [a] -> IO ()
playWithMPD logger reqData [] =
    getPlaylist reqData >>= playWithMPD logger reqData
playWithMPD logger reqData (x:xs) = do
    surl <- songUrl reqData x
    print surl
    when (surl /= "") $ do
        logAndReport logger reqData x
        mpdLoad $ fromString surl
        takeMVar eof                     -- Finished
    playWithMPD logger reqData xs
  where
    mpdLoad :: Path -> IO ()
    mpdLoad path = do
        withMPD $ do
            clear
            add path
        withMPD $ MPD.play Nothing

        mpdTag $ songMeta x
        mpdPlay
    mpdPlay :: IO ()
    mpdPlay = do
        st <- mpdState
        if st == Right Stopped
            then putMVar eof ()
            else mpdPlay

playWithMplayer :: Radio a => LoggerSet -> Param a -> [a] -> IO ()
playWithMplayer logger reqData [] =
    getPlaylist reqData >>= playWithMplayer logger reqData
playWithMplayer logger reqData (x:xs) = do
    surl <- songUrl reqData x
    when (surl /= "") $ do
        logAndReport logger reqData x
        let sh = "mplayer -cache 2048 -cache-min 5 -novideo " ++ surl
        void $ waitForProcess =<< runCommand sh
    playWithMplayer logger reqData xs

logAndReport :: Radio a => LoggerSet -> Param a -> a -> IO ()
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

-- "addtagid" command is available since mpd-0.19
mpdTag :: SongMeta -> IO ()
mpdTag meta = void $ withMPD $ do
    cs <- currentSong
    when (isJust cs) $ do
        let (Id sid) = fromJust $ sgId $ fromJust cs
        void $ do
            addTag sid arTag
            addTag sid alTag
            addTag sid tiTag
  where
    addTag sid tag = getResponse $ "addtagid " ++ show sid ++ tag
    arTag = " artist \"" ++ artist meta ++ "\""
    alTag = " album \"" ++ album meta ++ "\""
    tiTag = " title \"" ++ title meta ++ "\""

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
        yml <- getConfig
        exist <- doesFileExist yml
        bs <- if exist then B.readFile yml
                       else return ""
        let config = mkConfig tok
        B.writeFile yml $ B.append bs (encode config)
        putStrLn $ "Your token has been saved to " ++ yml

    mkParam :: Param a -> String -> Param a

    readToken :: FromJSON (Config a)
              => (Config a -> Param a) -> String -> IO (Maybe (Param a))
    readToken selector keywords = do
        yml <- getConfig
        exist <- doesFileExist yml
        if exist
            then do
                conf <- decodeFile yml
                case conf of
                    Nothing -> error $ "Invalid YAML file: " ++ show conf
                    Just c ->
                        case fromJSON c of
                            Success tok -> return $ Just $
                                mkParam (selector tok) keywords
                            Error err -> do
                                print $ "Parse token failed: " ++ show err
                                return Nothing
            else return Nothing

getLordDir :: IO FilePath
getLordDir = (++ "/.lord") <$> getHomeDirectory

getConfig :: IO FilePath
getConfig = (++ "/lord.yml") <$> getLordDir

getPidFile :: IO FilePath
getPidFile = (++ "/lord.pid") <$> getLordDir

getLogFile :: IO FilePath
getLogFile = (++ "/lord.log") <$> getLordDir

getStateFile :: IO FilePath
getStateFile = (++ "/lordstate") <$> getLordDir

formatLogMessage :: IO ZonedDate -> String -> IO LogStr
formatLogMessage getdate msg = do
    now <- getdate
    return $ toLogStr now <> " : " <> toLogStr msg <> "\n"

writeLog :: LoggerSet -> String -> IO ()
writeLog l msg = do
    (loggerDate, _) <- clockDateCacher
    formatLogMessage loggerDate msg >>= pushLogStr l
