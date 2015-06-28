{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Parser.ValueSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "X12.Parser.Value" $ do
    context "Parse a 8-digit date" $ do
      it "parses 20150627 into Day 2015-06-27" $ do
        ("20150627" :: Text) ~> parseDT
        `shouldParse` (DT (fromGregorian 2015 6 27))
    context "Parse a 6-digit date" $ do
      it "parses 150627 into Day 2015-06-27" $ do
        ("150627" :: Text) ~> parseDT
        `shouldParse` (DT (fromGregorian 2015 6 27))
    context "Parse a 6-digit time" $ do
      it "parses 123456 into TimeOfDay 12:34:56" $ do
        ("123456" :: Text) ~> parseTM
        `shouldParse` (TM (TimeOfDay 12 34 56))
    context "Parse a 4-digit time" $ do
      it "parses 1234 into TimeOfDay 12:34:00" $ do
        ("1234" :: Text) ~> parseTM
        `shouldParse` (TM (TimeOfDay 12 34 0))
