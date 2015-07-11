-- |

module X12.Tokens.SegmentToken where
import X12.Tokens.ElementToken
import Data.Text

data SegmentToken = SegmentToken { segmentTokenId :: Text
                                 , elementTokens :: [ElementToken]
                                 }
                  deriving (Eq, Show)
