-- | A generic interface to online radio services

module Radio
  ( SongMeta(..)
  , Radio(..)
  , getLordDir
  , getPid
  , getLogFile
  , getStateFile
  , writeLog
  ) where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Applicative ((<$>))
import           Control.Concurrent.MVar
import           Control.Concurrent (forkIO)
import           Control.Monad (when)
import           Data.Aeson (FromJSON, Value)
import qualified Data.ByteString.Char8 as C
import           Data.Conduit (runResourceT, ($$+-))
import           Network.MPD hiding (play, Value)
import qualified Network.MPD as MPD
import           System.Directory (getHomeDirectory)
import           System.IO (writeFile)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.FastLogger
import           System.Log.FastLogger.Date (ZonedDate)

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

    play :: Logger -> Param a -> [a] -> IO ()
    play logger reqData [] = getPlaylist reqData >>= play logger reqData
    play logger reqData (x:xs) = do
        surl <- songUrl reqData x
        print surl
        when (surl /= "") $ do
            let song = artist (songMeta x) ++ " - " ++ title (songMeta x)
            writeLog logger song 
            getStateFile >>= flip writeFile song
            mpdLoad $ Path $ C.pack surl
            takeMVar eof                     -- Finished
        play logger reqData xs

mpdLoad :: Path -> IO ()
mpdLoad path = do
    s <- withMPD $ do
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

getLordDir :: IO FilePath
getLordDir = (++ "/.lord") <$> getHomeDirectory

getPid :: IO FilePath
getPid = (++ "/lord.pid") <$> getLordDir

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
