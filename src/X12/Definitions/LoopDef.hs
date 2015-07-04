-- |

module X12.Definitions.LoopDef where
import Data.Text
import X12.Definitions.RepeatCount

data LoopDef = LoopDef { loopId :: Text
                       , loopRepeatCount :: RepeatCount
                       }
             deriving Show
