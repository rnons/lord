{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module for interfacing <http://radioreddit.com>
module Web.Radio.Reddit where

import           Control.Monad (liftM)
import           Data.Aeson
import           Data.Conduit (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit

import qualified Web.Radio as Radio

data Reddit = Reddit
    { title         :: String
    , artist        :: String
    , preview_url   :: String
    , download_url  :: Maybe String
    } deriving (Show, Generic)

instance FromJSON Reddit

instance Radio.Radio Reddit where
    data Param Reddit = Genre String

    parsePlaylist (Object hm) = do
        let songs = HM.lookup "songs" hm >>=
                    \(Object hm') -> HM.lookup "song" hm'
        case fromJSON $ fromMaybe Null songs of
            Success s -> s
            Error err -> error $ "Parse playlist failed: " ++ show err
    parsePlaylist _ = error "Unrecognized playlist format."

    getPlaylist (Genre g) = do
        let url = "http://radioreddit.com/api/" ++ g ++ "/status.json"
        req <- parseUrl url
        withManager $ \manager -> do
            res <- http req manager
            liftM Radio.parsePlaylist (responseBody res $$+- sinkParser json)

    songUrl _ x = return $ fromMaybe (preview_url x) (download_url x)

    songMeta x = Radio.SongMeta (artist x) "" (title x)

    tagged _ = False

genres :: IO [String]
genres = return 
    [ "main", "electronic", "rock", "metal"
    , "indie", "hiphop", "random", "talk" ]

pprGenres :: [String] -> IO ()
pprGenres [] = return ()
pprGenres gs = do
    putStrLn $ foldr1 f (take 4 gs)
    pprGenres $ drop 4 gs
  where
    f a b = a ++ concat (replicate (20 - length a) " ") ++ b
