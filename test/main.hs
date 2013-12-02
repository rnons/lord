{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromJust)
import Test.Hspec
import Test.HUnit

import Radio
import qualified Radio.Cmd as Cmd
import Radio.EightTracks
import Radio.Douban
import Radio.Jing
import qualified Radio.Reddit as Reddit


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "getPlaylist" $ do
    it "cmd: given genre" $ do
        ss <- getPlaylist (Cmd.Genre "Dream Pop")
        assertNotNull ss

    it "douban: given channel id" $ do
        ss <- getPlaylist (Cid 6)
        assertNotNull ss

    it "douban: given musician name" $ do
        ss <- getPlaylist (Musician "Sigur RóS")
        assertNotNull ss

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
