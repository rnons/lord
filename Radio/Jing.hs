{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module of http://jing.fm
-- It's a bit tricky to play jing.fm
-- Notice the `play` function of **instance Radio**
module Radio.Jing where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (liftM, mzero)
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import           Data.Yaml
import           Data.CaseInsensitive (mk)
import           Data.Conduit (runResourceT, ($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Data.Conduit.Attoparsec (sinkParser)
import           GHC.Generics (Generic)
import           Network.HTTP.Types 
import           Network.HTTP.Conduit
import           Network.MPD hiding (play, Value, Query)
import qualified Network.MPD as MPD
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Directory (doesFileExist)

import Radio

type Param a = Radio.Param Jing

downloaded = unsafePerformIO newEmptyMVar

data Jing = Jing 
    { abid :: Int       -- album id
    , aid  :: Int       -- artist id
    , an   :: String    -- album name
    , atn  :: String    -- artist name
    , d    :: String
    , fid  :: String
    , fs   :: Int       -- file size
    , mid  :: ByteString
    , n    :: String    -- song name
    , tid  :: Int
    --, y    :: Bool
    } deriving (Show, Generic)

instance FromJSON Jing

data Usr = Usr
    { userid :: Int
    , usernick :: ByteString
    } deriving Show

instance FromJSON Usr where
    parseJSON (Object v) = Usr <$>
                           v .: "id" <*>
                           v .: "nick"
    parseJSON _          = mzero

instance Radio.Radio Jing where
    data Param Jing = Token
        { aToken        :: ByteString
        , rToken        :: ByteString
        , uid           :: Int
        , nick          :: ByteString 
        , cmbt          :: String
        , highquality   :: Bool
        } deriving (Show, Generic)

    parsePlaylist (Object hm) = do
        let songs = HM.lookup "result" hm >>= 
                    \(Object hm') -> HM.lookup "items" hm'
        case fromJSON $ fromMaybe Null songs of
            Success s -> s
            Error err -> error $ "Parse playlist failed: " ++ show err
    parsePlaylist _ = error "Unrecognized playlist format."

    getPlaylist tok = do
        let url = "http://jing.fm/api/v1/search/jing/fetch_pls"
            query = [ ("q", C.pack $ encodeString $ cmbt tok)
                    , ("ps", "10")
                    , ("st", "0")
                    , ("u", C.pack $ show $ uid tok)
                    , ("tid", "0")
                    , ("mt", "")
                    , ("ss", "true")
                    ]
            aHdr = (mk "Jing-A-Token-Header", aToken tok) :: Header
            rHdr = (mk "Jing-R-Token-Header", rToken tok) :: Header

        initReq <- parseUrl url
        let req = initReq { requestHeaders = [aHdr, rHdr] }

        -- urlEncodeBody adds a content-type request header and
        -- changes the method to POST.
        let req' = urlEncodedBody query req
        withManager $ \manager -> do
            res <- http req' manager
            liftM Radio.parsePlaylist (responseBody res $$+- sinkParser json)

    songUrl tok x = do
        let url = "http://jing.fm/api/v1/media/song/surl"
            type_ = if highquality tok then "NO" else "MM"
            query = [ ("type", Just type_)
                    , ("mid", Just $ mid x)
                    ] :: Query
            aHdr = (mk "Jing-A-Token-Header", aToken tok) :: Header
            rHdr = (mk "Jing-R-Token-Header", rToken tok) :: Header

        initReq <- parseUrl url
        let req = initReq { method = "POST"
                          , requestHeaders = [aHdr, rHdr]
                          , queryString = renderQuery False query
                          }
        (Object hm) <- withManager $ \manager -> do
            res <- http req manager
            responseBody res $$+- sinkParser json
        let (String surl) = fromJust $ HM.lookup "result" hm
        return $ T.unpack surl

    songMeta x = Radio.SongMeta (atn x) (an x) (n x)

    -- Songs from jing.fm comes with tags!
    tagged _ = True

    -- The media file jing.fm provides is of m4a type.
    -- MPD is unable to stream m4a file, as a result, lord streams it to 
    -- ~/.lord/lord.m4a
    play logger reqData [] = Radio.getPlaylist reqData >>= Radio.play logger reqData
    play logger reqData (x:xs) = do
        surl <- Radio.songUrl reqData x
        print surl
        req <- parseUrl surl
        home <- Radio.getLordDir
        manager <- newManager def
        threadId <- forkIO $ E.catch 
            (do
                runResourceT $ do 
                    res <- http req manager
                    responseBody res $$+- sinkFile (home ++ "/lord.m4a")
                -- This will block until downloaded.

                let song = artist (songMeta x) ++ " - " ++ title (songMeta x)
                writeLog logger song 
                getStateFile >>= flip writeFile song

                putMVar downloaded ())
            (\e -> do
                print (e :: E.SomeException)
                Radio.writeLog logger $ show e
                Radio.play logger reqData xs
                )
        --mtid <- newMVar threadId
        threadDelay 3000000
        mpdLoad
        Radio.play logger reqData xs

mpdLoad :: IO ()
mpdLoad = do
    --m4a <- (++ "/lord.m4a") <$> Radio.getLordDir
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

instance FromJSON (Radio.Param Jing)
instance ToJSON (Radio.Param Jing)

login :: String -> IO (Radio.Param Jing)
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

createSession :: String -> String -> String -> IO (Maybe (Radio.Param Jing))
createSession keywords email pwd = do
    let url = "http://jing.fm/api/v1/sessions/create"
        query = [ ("email", C.pack email) , ("pwd", C.pack pwd) ]
    req <- parseUrl url
    let req' = urlEncodedBody query req
    res <- withManager $ \manager -> http req' manager
    let hmap = HM.fromList $ responseHeaders res
        atoken = HM.lookup "Jing-A-Token-Header" hmap
        rtoken = HM.lookup "Jing-R-Token-Header" hmap
        parseToken :: Value -> Maybe (Radio.Param Jing)
        parseToken (Object hm) = do
            let user = HM.lookup "result" hm >>= 
                       \(Object hm') -> HM.lookup "usr" hm'
            case fromJSON $ fromMaybe Null user of
                Success u -> Token <$> atoken
                                   <*> rtoken
                                   <*> (Just $ userid u)
                                   <*> (Just $ usernick u)
                                   <*> Just keywords
                                   <*> Just True
                Error err -> error $ "Retrieve token failed: " ++ show err
        parseToken _ = error "Unrecognized token format."
    liftM parseToken (runResourceT $ responseBody res $$+- sinkParser json)

saveToken :: Radio.Param Jing -> IO ()
saveToken tok = do
    home <- Radio.getLordDir
    let yml = home ++ "/lord.yml"
    encodeFile yml tok
    putStrLn "Your token has been saved to ~/lord.yml"

readToken :: String -> IO (Maybe (Radio.Param Jing))
readToken keywords = do
    home <- Radio.getLordDir
    let yml = home ++ "/lord.yml"
    exist <- doesFileExist yml
    if exist
       then do
            conf <- decodeFile yml
            case conf of
                Nothing -> error $ "Invalid YAML file: " ++ show conf
                Just c -> 
                    case fromJSON c of
                        Success tok -> return $ Just $ tok { cmbt = keywords }
                        Error err -> error $ "Parse token failed: " ++ show err
       else return Nothing
