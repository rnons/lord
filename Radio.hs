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
import           Control.Monad (when)
import           Data.Aeson (FromJSON, Value)
import qualified Data.ByteString.Char8 as C
import           Data.Conduit (runResourceT, ($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit
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

    play :: Logger -> Param a -> [a] -> IO ()
    play logger reqData [] = getPlaylist reqData >>= play logger reqData
    play logger reqData (x:xs)
      | playable x = do
        let 
            mpdLoad :: Path -> IO ()
            mpdLoad path = do
                withMPD $ do
                    clear
                    add path
                withMPD $ MPD.play Nothing
                mpdPlay

            mpdPlay :: IO ()
            mpdPlay = do
                withMPD $ idle [PlayerS]   
                -- This will block until paused/finished.

                st <- withMPD status
                let st' = fmap stState st
                print st'
                if st' == Right Stopped 
                    then putMVar eof ()
                    else mpdPlay

        surl <- songUrl reqData x
        print surl
        when (surl /= "") $ do
            let song = artist (songMeta x) ++ " - " ++ title (songMeta x)
            writeLog logger song 
            getStateFile >>= flip writeFile song
            mpdLoad $ Path $ C.pack surl
            takeMVar eof                     -- Finished
        play logger reqData xs
      | otherwise = do
        let 
            mpdLoad :: IO ()
            mpdLoad = do
                --m4a <- (++ "/lord.m4a") <$> getLordDir
                let m4a = "lord/lord.m4a"
                s <- withMPD $ do
                        clear
                        update [Path "lord"]
                        add m4a
                case s of
                    Right _ -> do
                        withMPD $ MPD.play Nothing
                        mpdPlay
                    _                  -> mpdLoad

            mpdPlay :: IO ()
            mpdPlay = do
                withMPD $ idle [PlayerS]   
                -- This will block until paused/finished.

                st <- withMPD status
                let st' = fmap stState st
                print st'
                bd <- isEmptyMVar eof
                if st' == Right Stopped 
                    then if bd 
                            then do                                     -- Slow Network
                                withMPD $ MPD.play Nothing
                                mpdPlay
                            else do
                                withMPD clear
                                takeMVar eof                     -- Finished
                    else mpdPlay                                        -- Pause

        surl <- songUrl reqData x
        print surl
        req <- parseUrl surl
        home <- getLordDir
        manager <- newManager def
        threadId <- forkIO $ E.catch 
            (do
                let song = artist (songMeta x) ++ " - " ++ title (songMeta x)
                getStateFile >>= flip writeFile song

                runResourceT $ do 
                    res <- http req manager
                    responseBody res $$+- sinkFile (home ++ "/lord.m4a")
                -- This will block until eof.

                writeLog logger song 
                putMVar eof ())
            (\e -> do
                print (e :: E.SomeException)
                writeLog logger $ show e
                play logger reqData xs
                )
        --mtid <- newMVar threadId
        threadDelay 3000000
        mpdLoad
        play logger reqData xs

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
