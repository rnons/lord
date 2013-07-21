{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Radio where

import           Control.Concurrent.MVar
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad (unless)
import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit
import           Data.Conduit.Binary
import           Network.HTTP.Conduit
import           Network.MPD hiding (play)
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
    , title     :: String
    }

class FromJSON a => Radio a where
    data Settings a :: *

    getPlaylist :: Settings a -> IO [a]

    songUrl :: Settings a -> a -> IO String

    songMeta :: a -> SongMeta

    tagged :: a -> Bool

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
                putStrLn (title $ songMeta x)
                
                -- To avoid file handle race, writeTag after download finished.
                unless (tagged x) $ do
                    let tag = setArtist (artist meta) 
                                $ setAlbum (album meta)
                                $ setTitle (title meta)
                                $ emptyID3Tag
                    writeTag (home ++ "/radio.m4a") tag
                    return ()

                -- Update song info (length)
                withMPD $ update $ [Path "radio"]
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
    s <- withMPD $ do
            clear
            update $ [Path "radio"]
            add "radio/radio.m4a"
    case s of
        Right [p] -> do
            withMPD $ MPD.play Nothing
            mpdPlay
        _                  -> mpdLoad

mpdPlay = do
    s <- withMPD $ idle [PlayerS]   -- block until paused/finished
    st <- withMPD status
    let st' = fmap stState st
    print st'
    bd <- isEmptyMVar d
    if st' == Right Stopped 
        then if bd 
                then do                                     -- Slow Network
                    withMPD $ MPD.play Nothing
                    mpdPlay
                else do
                    withMPD clear
                    takeMVar d                             -- Finished
        else mpdPlay                                        -- Pause

getRadioDir :: IO FilePath
getRadioDir = do
   home <- getHomeDirectory
   return $ home ++ "/.radio"

