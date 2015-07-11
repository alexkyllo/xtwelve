-- |

module X12.Definitions.SegmentDefs.GS where
import X12.Definitions.SegmentDef
import X12.Definitions.ElementDefs
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.ElementUse

gs = SegmentDef { segmentId = "GS"
                , segmentName = "Functional Group Header"
                , segmentPurpose = "To indicate the beginning of a functional group and to provider control information"
                , elementUses = [ ElementUse e479 Mandatory (Bounded 1)
                                , ElementUse e142 Mandatory (Bounded 1)
                                , ElementUse e373 Mandatory (Bounded 1)
                                , ElementUse e337 Mandatory (Bounded 1)
                                , ElementUse e28  Mandatory (Bounded 1)
                                , ElementUse e455 Mandatory (Bounded 1)
                                , ElementUse e480 Mandatory (Bounded 1)
                                ]
                }
