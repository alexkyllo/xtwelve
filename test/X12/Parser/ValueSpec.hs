{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Parser.ValueSpec where

import SpecHelper
import Data.Text (Text)
import Data.Time.Calendar (Day(..), fromGregorian)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "X12.Parser.Value" $ do
    context "Parse a 6-digit date" $ do
      it "parses 20150627 into Day 2015-06-27" $ do
        ("20150627" :: Text) ~> parseDT
        `shouldParse` (DT (fromGregorian 2015 6 27))
