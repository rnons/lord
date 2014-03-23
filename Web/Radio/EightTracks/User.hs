{-# LANGUAGE DeriveGeneric #-}

module Web.Radio.EightTracks.User where
import           Data.Aeson   (FromJSON)
import           GHC.Generics (Generic)

data User = User
    { user_token:: String
    , id        :: Int
    , login     :: String
    , web_path  :: String
    } deriving (Show, Generic)
instance FromJSON User

data Session = Session
    { user :: User
    , status :: String
    , errors :: Maybe String
    , notices :: Maybe String
    , logged_in :: Bool
    , api_version :: Int
    } deriving (Show, Generic)

instance FromJSON Session

