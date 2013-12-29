{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromJust)
import Test.Hspec
import Test.HUnit

import Web.Radio (getPlaylist, readToken)
import qualified Web.Radio.Cmd as Cmd
import Web.Radio.EightTracks
import Web.Radio.Douban
import Web.Radio.Jing
import qualified Web.Radio.Reddit as Reddit


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "getPlaylist" $ do
    it "cmd: given genre" $ do
        ss <- getPlaylist (Cmd.Genre "Dream Pop")
        assertNotNull ss

    it "douban: given channel id" $ do
        ss <- getPlaylist $ douban "6"
        assertNotNull ss

    it "douban: given album url" $ do
        ss <- getPlaylist $ douban "http://music.douban.com/subject/3044758/"
        assert $ albumtitle (head ss) == "變形記"

    it "douban: given musician url" $ do
        ss <- getPlaylist $ douban "http://music.douban.com/musician/104585/"
        assert $ artist (head ss) == "万能青年旅店"

    it "douban: given musician name" $ do
        ss <- getPlaylist $ douban "Sigur Rós"
        assert $ artist (head ss) == "Sigur Rós"

    it "8tracks: given mix id" $ do
        tok <- readToken eight "14"
        ss <- getPlaylist $ fromJust tok
        assertNotNull ss

    it "8tracks: given mix url" $ do
        mId <- getMixId "http://8tracks.com/an-nie/oblitus"
        tok <- readToken eight $ show mId
        ss <- getPlaylist $ fromJust tok
        assertNotNull ss

    it "jing: given keywords" $ do
        tok <- readToken jing "postrock"
        ss <- getPlaylist $ fromJust tok
        assertNotNull ss

    it "reddit: given genre" $ do
        ss <- getPlaylist (Reddit.Genre "indie")
        assertNotNull ss
  where
    assertNotNull = assert . not .null
