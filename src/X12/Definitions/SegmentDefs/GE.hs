-- |

module X12.Definitions.SegmentDefs.GE where
import X12.Definitions.SegmentDef
import X12.Definitions.ElementDefs
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.ElementUse

ge = SegmentDef { segmentId = "GE"
                , segmentName = "Functional Group Trailer"
                , segmentPurpose = "To indicate the end of a functional group and to provider control information"
                , elementUses = [ SimpleElementUse e97 Mandatory (Bounded 1)
                                , SimpleElementUse e28 Mandatory (Bounded 1)
                                ]
                }
