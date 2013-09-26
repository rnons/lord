-- | Module of http://cmd.fm

module Radio.Cmd where

import qualified Control.Exception as E
import           Control.Monad (liftM)
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Conduit (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as HM
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit
import           Network.HTTP.Types (renderQuery)
import           Network.HTTP.Types.Header (hLocation)
import           Network.HTTP.Types.Status (status302)

import qualified Radio

type Param a = Radio.Param Cmd

data Cmd = Cmd
    { sc_id :: Int
    , title :: String
    , artwork_url :: String
    , description   :: String
    , duration      :: Int
    , genre         :: String
    , tag_list      :: String
    , waveform_url  :: String
    , stream_url    :: String
    , last_listened :: Int
    , main_type     :: String
    --, created_at    :: String
    --, updated_at    :: String
    } deriving (Show, Generic)

instance FromJSON Cmd

instance Radio.Radio Cmd where
    data Param Cmd = Genre String

    parsePlaylist val = do
        case fromJSON val of
            Success s -> s
            Error err -> error $ "Parse playlist failed: " ++ show err

    getPlaylist (Genre g) = do
        let url = "http://cmd.fm/api/tracks/search"
            query = [ ("genre", Just $ C.pack g)
                    , ("limit", Just $ C.pack "10") ]
        initReq <- parseUrl url
        let req = initReq { method = "GET"
                          , queryString = renderQuery False query
                          }
        withManager $ \manager -> do
            res <- http req manager
            liftM Radio.parsePlaylist (responseBody res $$+- sinkParser json)

    songUrl _ x = do
        initReq <- parseUrl url
        let req = initReq { method = "GET"
                          , queryString = renderQuery False query
                          }
        E.catch (do withManager $ \manager -> 
                        httpLbs req { redirectCount = 0 } manager >> return "")
                (\e -> case e of
                    (StatusCodeException s hdr _) ->
                        if s == status302 then redirect hdr 
                        else return ""
                    otherException -> print otherException >> return ""
                )
      where
        url = stream_url x
        query = [("client_id", Just $ C.pack "2b659ea66970555922d89ce9c07b2d0d")]
        redirect hdr = case HM.lookup hLocation $ HM.fromList hdr of 
                            Just u   -> return $ C.unpack u
                            Nothing  -> return ""

    songMeta x = Radio.SongMeta "" "" (title x)

    tagged _ = False

genres :: IO [String]
genres = do 
    res <- simpleHttp "http://cmd.fm/get.php?genres=1"
    return $ fromMaybe [] (decode res :: Maybe [String])

pprGenres :: [String] -> IO ()
pprGenres [] = return ()
pprGenres gs = do
    putStrLn $ foldr1 f (take 3 gs)
    pprGenres $ drop 3 gs
  where
    f a b = a ++ (concat $ take (30 - length(a)) $ repeat " ") ++ b
