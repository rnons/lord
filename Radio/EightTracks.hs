{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Module of http://8tracks.com
-- API documentation: http://8tracks.com/developers/api_v3
module Radio.EightTracks where

import qualified Control.Exception as E
import           Control.Monad (forM_, liftM)
import           Control.Concurrent.MVar
import           Data.Aeson
import           Data.Aeson.Types (defaultOptions, Options(..))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (fromMaybe, fromJust)
import           Data.CaseInsensitive (mk)
import           Data.Char (isDigit)
import           Data.Conduit (($$+-))
import           Data.Conduit.Attoparsec (sinkParser, ParseError)
import qualified Data.List as L
import           GHC.Generics (Generic)
import           Network.HTTP.Types 
import           Network.HTTP.Conduit
import           Prelude hiding (id)
import           System.Console.ANSI
import           System.IO.Unsafe (unsafePerformIO)

import Radio
import qualified Radio.EightTracks.Explore as Exp
import qualified Radio.EightTracks.User as U


running :: MVar ()
running = unsafePerformIO newEmptyMVar

apiKey :: String
apiKey = "1de30eb2b8fe85b1740cfbee3fdbb928e2c7249b"

verHdr, keyHdr :: Header
verHdr = (mk "X-Api-Version", "3")
keyHdr = (mk "X-Api-Key", C.pack apiKey)

type ETParam = Radio.Param EightTracks

-- | Response from http://8tracks.com/sets/new.json
-- `play_token` is the value of interest
data PlaySession = PlaySession
    { play_token            :: String
    , status                :: String
    , errors                :: Maybe String
    , notices               :: Maybe String
    , api_version           :: Int
    } deriving (Show, Generic)
instance FromJSON PlaySession

-- | Response from /play.json /next.json
-- `track` is the value of interest
data PlayResponse = PlayResponse
    { play_set               :: MixSet
    , play_status            :: String
    } deriving (Show, Generic)
instance FromJSON PlayResponse where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

data MixSet = MixSet
    { at_beginning          :: Bool
    , at_last_track         :: Bool
    , at_end                :: Bool
    , skip_allowed          :: Bool
    , track                 :: EightTracks
    } deriving (Show, Generic)
instance FromJSON MixSet

data EightTracks = EightTracks 
    { id                    :: Int
    , track_file_stream_url :: String
    , name                  :: String
    , performer             :: String
    , release_name          :: Maybe String
    , url                   :: String
    } deriving (Show, Generic)
instance FromJSON EightTracks

-- | Response from http://8tracks.com/mixes/14.json or
-- http://8tracks.com/dp/electrominimalicious.json
-- `info_mix` is the value of interest
data MixInfo = MixInfo
    { info_mix               :: Exp.Mix
    , info_status            :: String
    } deriving (Show, Generic)
instance FromJSON MixInfo where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

instance Radio.Radio EightTracks where
    data Param EightTracks = Token
        { userToken     :: String
        , userName      :: String
        , playToken     :: Int
        , mixId         :: Int
        } deriving (Show, Generic)

    parsePlaylist val =
        case fromJSON val of
            Success s -> [track $ play_set s]
            Error err -> error $ "Parse playlist failed: " ++ show err

    -- Request play.json will record current mix to listening history.
    -- Request next.json won't.
    getPlaylist tok = E.catch
        (do
            justStarted <- isEmptyMVar running
            rurl <- if justStarted
                then do
                    putMVar running ()
                    return $ "http://8tracks.com/sets/" ++ show (playToken tok) ++ "/play.json"
                else
                    return $ "http://8tracks.com/sets/" ++ show (playToken tok) ++ "/next.json"
            getPlaylist' rurl)
        (\e -> do
            -- When reached the last track in this mix. Play it again
            print (e :: E.SomeException) 
            let rurl = "http://8tracks.com/sets/" ++ show (playToken tok) ++ "/play.json"
            getPlaylist' rurl)
      where
        usrHdr = (mk "X-User-Token", C.pack $ userToken tok)

        getPlaylist' rurl = do
            let query = [ ("mix_id", C.pack $ show $ mixId tok) ]

            initReq <- parseUrl rurl
            let req = initReq { requestHeaders = [verHdr, keyHdr, usrHdr] 
                              , queryString = renderSimpleQuery False query }
            withManager $ \manager -> do
                res <- http req manager
                liftM Radio.parsePlaylist (responseBody res $$+- sinkParser json)

    songUrl _ x = return $ track_file_stream_url x

    songMeta x = Radio.SongMeta (performer x) 
                                (fromMaybe "" $ release_name x) (name x)

    tagged _ = False
    
    reportRequired _ = True

    -- From api-doc: In order to be legal and pay royalties properly,
    -- at 30 seconds, report song played
    report tok x = do
        initReq <- parseUrl rurl
        let usrHdr = (mk "X-User-Token", C.pack $ userToken tok)
            req = initReq { requestHeaders = [verHdr, keyHdr, usrHdr] 
                          , queryString = renderSimpleQuery False query }
        res <- withManager $ \manager -> httpLbs req manager
        print $ responseBody res
      where
        rurl = "http://8tracks.com/sets/" ++ show (playToken tok) ++ "/report.json"
        query = [ ("track_id", C.pack $ show $ id x)
                , ("mix_id", C.pack $ show $ mixId tok) ]

instance FromJSON ETParam
instance ToJSON ETParam

instance NeedLogin EightTracks where
    createSession strMixId email pwd = do
        mId <- getMixId strMixId
        initReq <- parseUrl rurl
        let req = initReq { method = "POST"
                          , queryString = renderSimpleQuery False query
                          , requestHeaders = [verHdr] }
        res <- withManager $ \manager -> httpLbs req manager
        case eitherDecode $ responseBody res of
            Right r  -> do
                pTok <- newPlayToken
                return $ Just $ Token (U.user_token $ U.user r)
                                      (U.login $ U.user r) pTok mId
            Left err -> print err >> return Nothing
      where
        rurl = "http://8tracks.com/sessions.json"
        query = [ ("login", C.pack email), ("password", C.pack pwd) ]

    data Config EightTracks = Config { eight :: ETParam } deriving Generic

    mkConfig = Config

    mkParam param key = param { mixId = read key }

instance FromJSON (Radio.Config EightTracks)
instance ToJSON (Radio.Config EightTracks)

newPlayToken :: IO Int
newPlayToken = do
    res <- simpleHttp rurl
    let ses = fromJust (decode res :: Maybe PlaySession)
    return $ read $ play_token ses
  where
    rurl = "http://8tracks.com/sets/new.json?api_version=3&api_key=" ++ apiKey 

smartUrl :: String -> String
smartUrl smartId = 
    "http://8tracks.com/mix_sets/" ++ smartId ++ ".json?include=mixes"

smartGet :: String -> IO [Exp.Mix]
smartGet smartId = get (smartUrl smartId) (Exp.mixes . Exp.mix_set)

search :: String -> IO [Exp.Mix]
search [] = return []
search key = smartGet $ "keyword:" ++ key

featured, trending, newest :: IO [Exp.Mix]
featured = smartGet "collection:homepage"
trending = smartGet "all"
newest   = smartGet "all:recent"

pprMixes :: [Exp.Mix] -> IO ()
pprMixes mixes =
    forM_ mixes (\m -> do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr $ "* " ++ Exp.name m 
        setSGR [SetColor Foreground Vivid Green]
        putStr $ " id=" ++ show (Exp.id m)
        putStr $ "  ▶" ++ show (Exp.plays_count m)
        putStr $ "  ♥" ++ show (Exp.likes_count m)
        putStrLn $ "  (" ++ show (Exp.tracks_count m) ++ " tracks)"
        setSGR [Reset]
        putStrLn $ "    Description: " ++ 
                   unlines (map ("    " ++) (lines $ Exp.description m))
        putStrLn $ "    Tags: " ++ Exp.tag_list_cache m
        putStrLn ""
        )

get :: FromJSON a => String -> (a -> b) -> IO b
get rurl selector = do
    initReq <- parseUrl rurl
    let req = initReq { requestHeaders = [verHdr, keyHdr] }
    val <- E.catches 
        (withManager $ \manager -> do
            res <- http req manager
            responseBody res $$+- sinkParser json)
        [ E.Handler (\e -> print (e :: ParseError) >>
           error "The mix you are trying to access may be private.")
        , E.Handler (\e -> case e of
                        (StatusCodeException s _ _) ->
                            error $ show s
                        otherException ->
                            error $ show otherException) 
        ]

    case fromJSON val of
        Success v -> return $ selector v
        Error err -> error err

getMixId :: String -> IO Int
getMixId m
  | isNumerical m = return $ read m
  | otherwise = do
    let mixUrl = if domain `L.isPrefixOf` m
                     then m
                     else (domain ++)
                          (if "/" `L.isPrefixOf` m then m else '/' : m)
    get (mixUrl ++ ".json") (Exp.id . info_mix)
  where
    isNumerical = and . fmap isDigit
    domain = "http://8tracks.com"
