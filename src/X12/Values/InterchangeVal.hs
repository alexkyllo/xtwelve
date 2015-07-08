-- |

module X12.Values.InterchangeVal where
import X12.Definitions.InterchangeDef
import X12.Values.SegmentVal
import X12.Separators

data InterchangeVal =
  InterchangeVal { interchangeDef :: InterchangeDef
                 , children :: [SegmentVal]
                 , separators :: Separators
                 }
  deriving Show
