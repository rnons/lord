{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A generic interface to online radio services
module Radio
  ( SongMeta(..)
  , Radio(..)
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
import           Data.Aeson (FromJSON, Value)
import qualified Data.ByteString.Char8 as C
import           Data.Conduit (runResourceT, ($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit hiding (path)
import           Network.MPD hiding (play, Value)
import qualified Network.MPD as MPD
import           System.Directory (getHomeDirectory)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.FastLogger

eof :: MVar ()
eof = unsafePerformIO newEmptyMVar

data SongMeta = SongMeta 
    { artist    :: String
    , album     :: String
    , title     :: String
    }

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
                    then threadDelay (5*10^6) >> reportLoop param x
                    else report param x
            Left err -> print err

    play :: Logger -> Param a -> [a] -> IO ()
    play logger reqData [] = getPlaylist reqData >>= play logger reqData
    play logger reqData (x:xs)
      | playable x = do
        surl <- songUrl reqData x
        print surl
        when (surl /= "") $ do
            let song = artist (songMeta x) ++ " - " ++ title (songMeta x)
            writeLog logger song 
            getStateFile >>= flip writeFile song
            -- Report song played if needed
            when (reportRequired x) $ void (forkIO $ reportLoop reqData x)
            mpdLoad $ Path $ C.pack surl
            takeMVar eof                     -- Finished
        play logger reqData xs
      | otherwise = do
        surl <- songUrl reqData x
        print surl
        req <- parseUrl surl
        home <- getLordDir
        manager <- newManager def
        forkIO $ E.catch 
            (do
                let song = artist (songMeta x) ++ " - " ++ title (songMeta x)
                writeLog logger song 
                getStateFile >>= flip writeFile song
                -- Report song played if needed
                when (reportRequired x) $ void (forkIO $ reportLoop reqData x)

                runResourceT $ do 
                    res <- http req manager
                    responseBody res $$+- sinkFile (home ++ "/lord.m4a")
                -- This will block until eof.

                putMVar eof ())
            (\e -> do
                print (e :: E.SomeException)
                writeLog logger $ show e
                play logger reqData xs
                )
        threadDelay (3*10^6)
        mpdLoad m4a
        play logger reqData xs
      where
        m4a = "lord/lord.m4a"

        mpdLoad :: Path -> IO ()
        mpdLoad path
          | playable x = do
            withMPD $ do
                clear
                add path
            withMPD $ MPD.play Nothing
            mpdPlay
          | otherwise = do
            s <- withMPD $ do
                    clear
                    update [Path "lord"]
                    add path
            case s of
                Right _ -> do
                    withMPD $ MPD.play Nothing
                    mpdPlay
                _                  -> mpdLoad path

        mpdState :: IO (MPD.Response State)
        mpdState = do
            withMPD $ idle [PlayerS]   
            -- This will block until paused/finished.

            st <- liftM stState <$> withMPD status
            print st
            return st

        mpdPlay :: IO ()
        mpdPlay
          | playable x = do
            st <- mpdState
            if st == Right Stopped 
                then putMVar eof ()
                else mpdPlay
          | otherwise = do
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
