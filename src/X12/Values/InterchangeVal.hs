-- |

module X12.Values.InterchangeVal where
import X12.Definitions.InterchangeDef
import X12.Values.SegmentVal

data InterchangeVal =
  InterchangeVal { interchangeDef :: InterchangeDef
                 , children :: [SegmentVal]
                 }
  deriving Show
