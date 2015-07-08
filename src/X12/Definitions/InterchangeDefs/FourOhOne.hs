-- |

module X12.Definitions.InterchangeDefs.FourOhOne where
import X12.Definitions.SegmentUse
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.SegmentDefs.ISA
import X12.Definitions.InterchangeDef


fourOhOne = InterchangeDef { interchangeDefId = "00401"
                           , headerSegmentUses = [isaUse]
                           , trailerSegmentUses = []
                           }

isaUse = SegmentUse { segmentUseDef = isa
                    , segmentReq = Mandatory
                    , segmentRepeatCount = Bounded 1
                    , segmentParent = Nothing
                    }
