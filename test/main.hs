{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromJust)
import Test.Hspec
import Test.HUnit

import Radio
import qualified Radio.Cmd as Cmd
import qualified Radio.EightTracks as ET
import Radio.Douban
import Radio.Jing
import qualified Radio.Reddit as Reddit


main :: IO ()
main = do
    hspec spec

spec :: Spec
spec = do
    describe "getPlaylist" $ do
        it "cmd: given genre" $ do
            ss <- Radio.getPlaylist (Cmd.Genre "Dream Pop")
            assert $ length ss > 0

        it "douban: given channel id" $ do
            ss <- Radio.getPlaylist (Cid 6)
            assert $ length ss > 0
    
        it "douban: given musician name" $ do
            ss <- Radio.getPlaylist (Musician "Sigur RóS")
            assert $ length ss > 0

        it "8tracks: given mix id" $ do
            tok <- readToken "14" :: IO (Maybe (Radio.Param ET.EightTracks))
            ss <- Radio.getPlaylist $ fromJust tok
            assert $ length ss > 0

        it "jing: given keywords" $ do
            tok <- readToken "postrock" :: IO (Maybe (Radio.Param Jing))
            ss <- Radio.getPlaylist $ fromJust tok
            assert $ length ss > 0

        it "reddit: given genre" $ do
            ss <- Radio.getPlaylist (Reddit.Genre "indie")
            assert $ length ss > 0
