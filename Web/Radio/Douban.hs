{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module of http://douban.fm
module Web.Radio.Douban where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Char (isDigit)
import           Data.Conduit (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as HM
import           Data.List (isPrefixOf)
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types (urlEncode, renderQuery, Query)
import           Prelude hiding (id)
import           System.Console.ANSI
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor

import qualified Web.Radio as Radio


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
    let rurl = "http://douban.fm/j/mine/playlist"
    initReq <- parseUrl rurl
    let req = initReq { method = "GET"
                      , queryString = renderQuery False query
                      }
    withManager $ \manager -> do
        res <- http req manager
        liftM Radio.parsePlaylist (responseBody res $$+- sinkParser json)

musicianId :: String -> IO (Maybe String)
musicianId mname = do
    let rurl = "http://music.douban.com/subject_search/?search_text=" ++
              C.unpack (urlEncode True (C.pack $ encodeString mname))
    rsp <- simpleHttp rurl
    let cursor = fromDocument $ parseLBS rsp
        href = cursor $// element "a"
                      >=> attributeIs "class" "ll musician_title "
                      &|  attribute "href"
    return $ Just $ filter isDigit $ T.unpack $ head $ head href

albumPlayable :: Int -> IO Bool
albumPlayable aId = do
    res <- simpleHttp $ aPattern ++ show aId
    let cursor = fromDocument $ parseLBS res
        start_radio = cursor $// element "div"
                             >=> attributeIs "class" "start_radio"
    return $ not $ null start_radio
  where
    aPattern = "http://music.douban.com/subject/"

mkQuery :: Int -> String -> Query
mkQuery cid context =
    [ ("type", Just "n")
    , ("channel", Just $ C.pack $ show cid)
    , ("context", Just $ C.pack context)
    , ("from", Just "lord")
    ]

instance Radio.Radio Douban  where
    data Param Douban = ChannelId Int
                      | Album Int
                      | MusicianId Int
                      | MusicianName String

    -- TODO: those without ssid filed are ads, filter them out!
    parsePlaylist (Object hm) = do
        let songs = HM.lookup "song" hm
        case fromJSON $ fromMaybe Null songs of
             Success s -> s
             Error _   -> []
    parsePlaylist _ = error "Unrecognized playlist format."

    getPlaylist (ChannelId cid) = getPlaylist' $ mkQuery cid ""
    getPlaylist (Album aId) = do
        playable <- albumPlayable aId
        if playable then getPlaylist' $
                         mkQuery 0 $ "channel:0|subject_id:" ++ show aId
                    else error "This album can not be played."
    getPlaylist (MusicianId mid) =
        getPlaylist' $ mkQuery 0 $ "channel:0|musician_id" ++ show mid
    getPlaylist (MusicianName mname) = do
        mmid <- musicianId mname
        Radio.getPlaylist (MusicianId $ read $ fromJust mmid)

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
hot = search' rurl
  where
    rurl = "http://douban.fm/j/explore/hot_channels"


-- | Return a list of up trending channels.
trending :: IO [Channel]
trending = search' rurl
  where
    rurl = "http://douban.fm/j/explore/up_trending_channels"


-- | Return a list of channels matching provided keywords.
search :: String -> IO [Channel]
search [] = return []
search key = search' rurl
  where
    rurl = "http://douban.fm/j/explore/search?query=" ++
          -- encodeString: encode chinese characters
          C.unpack (urlEncode True (C.pack $ encodeString key))


search' :: String -> IO [Channel]
search' rurl = do
    req <- parseUrl rurl
    (Object hm) <- withManager $ \manager -> do
        res <- http req manager
        responseBody res $$+- sinkParser json

    let (Object hm') = fromJust $ HM.lookup "data" hm
        resData = fromJust $ HM.lookup "channels" hm'
        channels = fromJSON resData :: Result [Channel]
    case channels of
        Success c -> return c
        Error err -> putStrLn err >> print resData >> return []

douban :: String -> Radio.Param Douban
douban k
    | isChId k = ChannelId $ read k
    | aPattern `isPrefixOf` k = Album $ read $ init $ drop (length aPattern) k
    | mPattern `isPrefixOf` k = MusicianId $ read $ init $ drop (length mPattern) k
    | otherwise = MusicianName k
  where
    isChId = and . fmap isDigit
    aPattern = "http://music.douban.com/subject/"
    mPattern = "http://music.douban.com/musician/"
