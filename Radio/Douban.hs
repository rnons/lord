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
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types (urlEncode, renderQuery, Query)
import           Prelude hiding (id)
import           System.Console.ANSI
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor

import qualified Radio

type Param a = Radio.Param Douban

data Douban = Douban 
    { picture :: String
    , albumtitle :: String
    -- , company :: String
    -- , rating_avg :: Float
    -- , public_time :: String
    , ssid :: Maybe String
    , album :: String
    -- , like :: Int
    , artist :: String
    , url :: String
    , title :: String
    , subtype :: String
    -- , length :: Int
    , sid :: String
    , aid :: String
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


getPlaylist' :: Query -> IO [Douban]
getPlaylist' query = do
    let url = "http://douban.fm/j/mine/playlist"
    initReq <- parseUrl url
    let req = initReq { method = "GET"
                      , queryString = renderQuery False query
                      }
    withManager $ \manager -> do
        res <- http req manager
        liftM Radio.parsePlaylist (responseBody res $$+- sinkParser json)

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

    -- TODO: those without ssid filed are ads, filter them out!
    parsePlaylist (Object hm) = do
        let songs = HM.lookup "song" hm
        case fromJSON $ fromMaybe Null songs of
             Success s -> s
             Error err -> []

    getPlaylist (Cid cid) = do
        let query = [ ("type", Just "n")
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

    songUrl _ x = return $ url x

    songMeta x = Radio.SongMeta (artist x) (albumtitle x) (title x)

    -- Songs from douban.fm comes with no tags!
    tagged _ = False

data Channel = Channel 
    { intro :: String
    , name :: String
    , song_num :: Int
    -- , creator :: Creator
    , banner :: String
    , cover :: String
    , id :: Int
    , hot_songs :: [String]
    } deriving (Eq, Show)

instance FromJSON Channel where
    parseJSON (Object v) = Channel <$>
                           v .: "intro" <*>
                           v .: "name" <*>
                           v .: "song_num" <*>
                           v .: "banner" <*>
                           v .: "cover" <*>
                           v .: "id" <*>
                           v .: "hot_songs"
    parseJSON _          = mzero


pprChannels :: [Channel] -> IO ()
pprChannels chs =
    forM_ chs (\c -> do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr $ "* " ++ name c 
        setSGR [SetColor Foreground Vivid Green]
        putStrLn $ " cid=" ++ show (id c)
        setSGR [Reset]
        let folding = foldr (\x acc -> 
                      if x `elem` "\r\n" then ' ':acc else x:acc) []
        putStrLn $ "    Intro: " ++ folding (intro c) 
        putStr "    Hot songs: " 
        forM_ (hot_songs c) (\s -> putStr $ s ++ ", ")
        putStrLn ""
        )

-- | Return a list of hot channels.
hot :: IO [Channel]
hot = search' url
  where
    url = "http://douban.fm/j/explore/hot_channels"


-- | Return a list of up trending channels.
trending :: IO [Channel]
trending = search' url
  where
    url = "http://douban.fm/j/explore/up_trending_channels"


-- | Return a list of channels matching provided keywords.
search :: String -> IO [Channel]
search [] = return []
search key = search' url
  where 
    url = "http://douban.fm/j/explore/search?query=" ++ 
          -- encodeString: encode chinese characters
          (C.unpack $ urlEncode True (C.pack $ encodeString key))


search' :: String -> IO [Channel]
search' url = do
    req <- parseUrl url
    (Object hm) <- withManager $ \manager -> do
        res <- http req manager
        responseBody res $$+- sinkParser json

    let (Object hm') = fromJust $ HM.lookup "data" hm
        resData = fromJust $ HM.lookup "channels" hm'
        channels = fromJSON resData :: Result [Channel]
    case channels of
        Success c -> return c
        Error err -> putStrLn err >> print resData >> return []
