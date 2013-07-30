{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Radio where

import           Control.Concurrent.MVar
import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import           Control.Monad (unless)
import           Data.Aeson (FromJSON)
import           Data.Conduit (runResourceT, ($$+-))
import           Data.Conduit.Binary (sinkFile)
import           ID3.Simple
import           ID3.Type.Tag (emptyID3Tag)
import           Network.HTTP.Conduit
import           Network.MPD hiding (play)
import qualified Network.MPD as MPD
import           System.Directory (getHomeDirectory)
import           System.IO.Unsafe (unsafePerformIO)

downloaded = unsafePerformIO newEmptyMVar

data SongMeta = SongMeta 
    { artist    :: String
    , album     :: String
    , title     :: String
    }

class FromJSON a => Radio a where
    data Param a :: *

    getPlaylist :: Param a -> IO [a]

    songUrl :: Param a -> a -> IO String

    songMeta :: a -> SongMeta

    tagged :: a -> Bool

    play :: Param a -> [a] -> IO ()
    play reqData [] = getPlaylist reqData >>= play reqData
    play reqData (x:xs) = do
        surl <- songUrl reqData x
        print surl
        req <- parseUrl surl
        home <- getRadioDir
        manager <- newManager def
        threadId <- forkIO $ E.catch 
            (do
                runResourceT $ do 
                    res <- http req manager
                    responseBody res $$+- sinkFile (home ++ "/radio.m4a")
                -- This will block until downloaded.

                putStrLn (artist $ songMeta x)
                putStrLn (title $ songMeta x)
                
                -- writeTag after downloaded to avoid file handle race.
                unless (tagged x) $ do
                    let meta = songMeta x
                        tag = setArtist (artist meta) 
                                $ setAlbum (album meta)
                                $ setTitle (title meta)
                                $ emptyID3Tag
                    writeTag (home ++ "/radio.m4a") tag
                    
                    -- Update song info
                    withMPD $ update [Path "lord"]
                    return ()

                putMVar downloaded ())
            (\e -> do
                print (e :: HttpException)
                Radio.play reqData xs
                )
                
        --mtid <- newMVar threadId
        threadDelay 3000000
        mpdLoad
        play reqData xs

mpdLoad :: IO ()
mpdLoad = do
    s <- withMPD $ do
            clear
            update [Path "lord"]
            add "lord/radio.m4a"
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
    bd <- isEmptyMVar downloaded
    if st' == Right Stopped 
        then if bd 
                then do                                     -- Slow Network
                    withMPD $ MPD.play Nothing
                    mpdPlay
                else do
                    withMPD clear
                    takeMVar downloaded                     -- Finished
        else mpdPlay                                        -- Pause

getRadioDir :: IO FilePath
getRadioDir = do
   home <- getHomeDirectory
   return $ home ++ "/.lord"

