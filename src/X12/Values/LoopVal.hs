-- |

module X12.Values.LoopVal where
import X12.Values.SegmentVal

data SegmentLoopVal = SegmentVal | LoopVal

data LoopVal = LoopVal { loopDef :: LoopDef
                       , children :: SegmentLoopVal
                       }
