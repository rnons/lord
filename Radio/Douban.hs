{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Radio.Douban where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Char (isDigit)
import           Data.Conduit (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types (urlEncode, renderQuery)
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor

import qualified Radio

type Param a = Radio.Param Douban

data Douban = Douban {
        picture :: String,
        albumtitle :: String,
        --company :: String,
        --rating_avg :: Float,
        --public_time :: String,
        ssid :: Maybe String,
        album :: String,
        --like :: Int,
        artist :: String,
        url :: String,
        title :: String,
        subtype :: String,
        --length :: Int,
        sid :: String,
        aid :: String
    } deriving Show

instance FromJSON Douban where
    parseJSON (Object v) = Douban <$>
                           v .: "picture" <*>
                           v .: "albumtitle" <*>
                           v .:? "ssid" <*>
                           v .: "album" <*>
                           --v .: "like" <*>
                           v .: "artist" <*>
                           v .: "url" <*>
                           v .: "title" <*>
                           v .: "subtype" <*>
                           v .: "sid" <*>
                           v .: "aid"
    parseJSON _          = mzero


getPlaylist' query = do
    let url = "http://douban.fm/j/mine/playlist"
    initReq <- parseUrl url
    let req = initReq { method = "GET"
                      , queryString = renderQuery False query
                      }
    (Object hm) <- withManager $ \manager -> do
        response <- http req manager
        responseBody response $$+- sinkParser json

    -- TODO: those without ssid filed are ads, filter them out!
    let songs = fromJust $ HM.lookup "song" hm
        pls = fromJSON songs :: Result [Douban]
    case pls of
        Success s -> return s
        Error err -> putStrLn err >> print songs >> return []

musicianID :: String -> IO (Maybe String)
musicianID name = do
    let url = "http://music.douban.com/search/" ++ 
              (C.unpack $ urlEncode True (C.pack $ encodeString name))
    rsp <- simpleHttp url
    let cursor = fromDocument $ parseLBS rsp
        href = cursor $// element "a" 
                      >=> attributeIs "class" "ll musician_title "
                      &|  attribute "href"
    return $ Just $ filter isDigit $ T.unpack $ head $ head href

instance Radio.Radio Douban  where
    data Param Douban = Cid Int | Musician String

    getPlaylist (Cid cid) = do
        let url = "http://douban.fm/j/mine/playlist"
            query = [ ("type", Just "n")
                    , ("channel", Just $ C.pack $ show cid)
                    , ("from", Just "lord")
                    ]
        getPlaylist' query
    getPlaylist (Musician name) = do
        mId <- musicianID name
        let query = [ ("type", Just "n")
                    , ("channel", Just "0")
                    , ("context", Just $ C.pack ("channel:0|musician_id:" ++ fromJust mId))
                    , ("from", Just "lord")
                    ]
        getPlaylist' query

    songUrl dou x = return $ url x

    songMeta x = Radio.SongMeta (artist x) (albumtitle x) (title x)

    -- Songs from douban.fm comes with no tags!
    tagged x = False


-- Radio.play (Cid 6) []
-- Radio.play (Musician "Sigur Ros") []
