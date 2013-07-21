{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Radio where

import           Control.Concurrent.MVar
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad (when)
import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit
import           Data.Conduit.Binary
import           Network.HTTP.Conduit
import qualified Network.MPD as MPD
import           System.Directory (getHomeDirectory)
import           System.IO.Unsafe (unsafePerformIO)
import ID3.Simple
import ID3.Type.Tag
import Data.Maybe

d = unsafePerformIO newEmptyMVar

data SongMeta = SongMeta 
    { artist    :: String
    , album     :: String
    , songname      :: String
    }

class FromJSON a => Radio a where
    data Settings a :: *
    getPlaylist :: Settings a -> IO [a]
    songUrl :: Settings a -> a -> IO String

    songMeta :: a -> SongMeta

    play :: Settings a -> [a] -> IO ()
    play reqData [] = Radio.getPlaylist reqData >>= Radio.play reqData
    play reqData (x:xs) = do
        let meta = songMeta x
        surl <- songUrl reqData x
        print surl
        req <- parseUrl surl
        home <- getRadioDir
        manager <- newManager def
        threadId <- forkIO $ E.catch 
            (do
                runResourceT $ do 
                    response <- http req manager
                    responseBody response $$+- sinkFile (home ++ "/radio.m4a")
                putStrLn (artist $ songMeta x)
                putStrLn (songname $ songMeta x)
                
                -- To avoid file handle race, readTag after download finished.
                (Right song) <- MPD.withMPD MPD.currentSong
                let tag = MPD.sgGetTag MPD.Artist (fromJust song)
                when (tag == Nothing) $ do
                    let tag = setArtist (artist meta) emptyID3Tag
                        tag' = setTitle (songname meta) tag
                    writeTag (home ++ "/radio.m4a") tag'
                    return ()

                -- Update song info (length)
                MPD.withMPD $ MPD.update $ [MPD.Path "radio"]
                putMVar d ())
            (\e -> do
                print (e :: HttpException)
                Radio.play reqData xs
                )
                --next)
        --mtid <- newMVar threadId
        threadDelay 3000000
        mpdLoad
        Radio.play reqData xs

mpdLoad = do
    s <- MPD.withMPD $ do
            MPD.clear
            MPD.update $ [MPD.Path "radio"]
            MPD.add "radio/radio.m4a"
    case s of
        Right [p] -> do
            MPD.withMPD $ MPD.play Nothing
            mpdPlay
        _                  -> mpdLoad

mpdPlay = do
    s <- MPD.withMPD $ MPD.idle [MPD.PlayerS]   -- block until paused/finished
    st <- MPD.withMPD MPD.status
    let st' = fmap MPD.stState st
    print st'
    bd <- isEmptyMVar d
    if st' == Right MPD.Stopped 
        then if bd 
                then do                                     -- Slow Network
                    MPD.withMPD $ MPD.play Nothing
                    mpdPlay
                else takeMVar d                             -- Finished
        else mpdPlay                                        -- Pause

getRadioDir :: IO FilePath
getRadioDir = do
   home <- getHomeDirectory
   return $ home ++ "/.radio"

