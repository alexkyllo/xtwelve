-- |

module X12.ParserSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "X12.Parser" $ do
    context "Parse an InterchangeVal" $ do
      it "parses a [SegmentToken] into an InterchangeVal" $ do
        pendingWith "need to update Parser to work with SegmentToken i/o SegmentTok"
