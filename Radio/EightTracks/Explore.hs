{-# LANGUAGE DeriveGeneric #-}

module Radio.EightTracks.Explore where
import           Data.Aeson   (FromJSON)
import           GHC.Generics (Generic)

data Mix = Mix
    { id        :: Int
    , path      :: String
    , web_path  :: String
    , name      :: String
    , description :: String
    , tag_list_cache :: String
    } deriving (Show, Generic)

instance FromJSON Mix

data MixSet = MixSet
    { mixes     :: [Mix]
    , smart_id  :: String
    , smart_type:: String
    } deriving (Show, Generic)

instance FromJSON MixSet
    
data MixExplore = MixExplore
    { mix_set   :: MixSet
    , status    :: String
    } deriving (Show, Generic)

instance FromJSON MixExplore

