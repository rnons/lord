{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Radio.Jing where

import Prelude hiding (id)
import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Data
import Data.Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import Data.CaseInsensitive
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Types
import Network.HTTP.Conduit
import qualified Data.HashMap.Strict     as HM
import Data.ByteString (ByteString)

import Radio

type Settings a = Radio.Settings Jing

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
    } deriving Generic

instance FromJSON Jing

instance Radio.Radio Jing where
    data Settings Jing = Token
        { aToken    :: ByteString
        , rToken    :: ByteString
        , uid       :: ByteString
        , nick      :: ByteString 
        , cmbt      :: ByteString
        } deriving (Show)

    getPlaylist tok = do
        let url = "http://jing.fm/api/v1/search/jing/fetch_pls"
            query = [ ("q", Just $ cmbt tok)
                    , ("ps", Just "5")
                    , ("st", Just "0")
                    , ("u", Just $ uid tok)
                    , ("tid", Just "0")
                    , ("mt", Nothing)
                    , ("ss", Just "true")
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
        let (Object hm') = fromJust $ HM.lookup "result" hm
        let songs = fromJust $ HM.lookup "items" hm'
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

    tagged x = True
