-- |

module X12.Values.SegmentVal where
import Data.Text
import X12.Values.ElementVal

data SegmentVal =
  SegmentVal { segmentValId :: Text
             , elementVals :: [ElementVal]
             }
  deriving Show
