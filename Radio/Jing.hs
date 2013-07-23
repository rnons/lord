{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Radio.Jing where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Codec.Binary.UTF8.String (encodeString)
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.Configurator as CF
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Data.CaseInsensitive (mk)
import           Data.Conduit (runResourceT, ($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import           Network.HTTP.Types 
import           Network.HTTP.Conduit
import           System.IO
import           System.Directory (doesFileExist)

import Radio

type Param a = Radio.Param Jing

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
        { aToken    :: ByteString
        , rToken    :: ByteString
        , uid       :: ByteString
        , nick      :: ByteString 
        , cmbt      :: String
        } deriving (Show)

    getPlaylist tok = do
        let url = "http://jing.fm/api/v1/search/jing/fetch_pls"
            query = [ ("q", C.pack $ encodeString $ cmbt tok)
                    , ("ps", "5")
                    , ("st", "0")
                    , ("u", uid tok)
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
        (Object hm) <- withManager $ \manager -> do
            res <- http req' manager
            responseBody res $$+- sinkParser json

        let (Object hm') = fromJust $ HM.lookup "result" hm
            songs = fromJust $ HM.lookup "items" hm'
            pls = fromJSON songs :: Result [Jing]
        case pls of
            Success s -> return s
            Error err -> putStrLn err >> print songs >> return []

    songUrl tok x = do
        let url = "http://jing.fm/api/v1/media/song/surl"
            query = [ ("type", Just "NO")
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
    tagged x = True

login :: String -> IO (Radio.Param Jing)
login cmbt = do
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
    mtoken <- createSession cmbt email pwd
    case mtoken of
         Just tok -> do
             saveToken tok
             return tok
         Nothing  -> do
             putStrLn "ERROR: Invalid email or password!"
             login cmbt

createSession :: String -> String -> String -> IO (Maybe (Radio.Param Jing))
createSession cmbt email pwd = do
    let url = "http://jing.fm/api/v1/sessions/create"
        query = [ ("email", C.pack email) , ("pwd", C.pack pwd) ]
    req <- parseUrl url
    let req' = urlEncodedBody query req
    res <- withManager $ \manager -> http req' manager
    let hmap = HM.fromList $ responseHeaders res
        atoken = HM.lookup "Jing-A-Token-Header" hmap
        rtoken = HM.lookup "Jing-R-Token-Header" hmap
    (Object hm) <- runResourceT $ responseBody res $$+- sinkParser json

    let (Object hm') = fromJust $ HM.lookup "result" hm
    case HM.lookup "result" hm of
        Just (Object hm') -> do
            let user = fromJust $ HM.lookup "usr" hm'
                user' = fromJSON user :: Result Usr
            case user' of
                Success u -> do
                    return $ Token <$> atoken
                                   <*> rtoken
                                   <*> (Just $ C.pack $ show $ userid u)
                                   <*> (Just $ usernick u)
                                   <*> Just cmbt
                Error err -> putStrLn err >> print user >> return Nothing
        _         -> return Nothing

saveToken :: Radio.Param Jing -> IO ()
saveToken tok = do
    home <- Radio.getRadioDir
    writeFile (home ++ "/lord.cfg") $ pprToken tok
    putStrLn "Your token has been saved to ~/lord.cfg"

pprToken tok = unlines [ "token"
                       , "{"
                       , "    atoken = \"" ++ C.unpack (aToken tok) ++ "\""
                       , "    rtoken = \"" ++ C.unpack (rToken tok) ++ "\""
                       , "    uid    = \"" ++ C.unpack (uid tok) ++ "\""
                       , "    nick   = \"" ++ C.unpack (nick tok) ++ "\""
                       , "}" ]

readToken :: String -> IO (Maybe (Radio.Param Jing))
readToken cmbt = do
    home <- Radio.getRadioDir
    let path = home ++ "/lord.cfg"
    exist <- doesFileExist path
    if exist
       then do
            conf <- CF.load [ CF.Required path ]
            atoken <- CF.lookup conf "token.atoken" :: IO (Maybe ByteString)
            rtoken <- CF.lookup conf "token.rtoken" :: IO (Maybe ByteString)
            uid <- CF.lookup conf "token.uid" :: IO (Maybe ByteString)
            nick <- CF.lookup conf "token.nick" :: IO (Maybe ByteString)
            return $ Token <$> atoken 
                           <*> rtoken 
                           <*> uid 
                           <*> nick 
                           <*> Just cmbt
       else return Nothing
