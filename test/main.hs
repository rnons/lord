{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromJust)
import Test.Hspec
import Test.HUnit

import Radio
import Radio.Cmd
import Radio.Jing
import Radio.Douban


main :: IO ()
main = do
    hspec spec

spec :: Spec
spec = do
    describe "getPlaylist" $ do
        it "cmd: given genre" $ do
            ss <- Radio.getPlaylist (Genre "Dream Pop")
            assert $ length ss > 0

        it "douban: given channel id" $ do
            ss <- Radio.getPlaylist (Cid 6)
            assert $ length ss > 0
    
        it "douban: given musician name" $ do
            ss <- Radio.getPlaylist (Musician "Sigur RóS")
            assert $ length ss > 0

        it "jing" $ do
            tok <- readToken "postrock"
            ss <- Radio.getPlaylist $ fromJust tok
            assert $ length ss > 0
