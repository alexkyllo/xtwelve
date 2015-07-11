-- |

module X12.Values.LoopVal where
import X12.Definitions.LoopDef
import X12.Values.SegmentVal

data SegmentLoopVal = SegmentVal | LoopVal { loopDef :: LoopDef
                                           , children :: SegmentLoopVal
                                           }
