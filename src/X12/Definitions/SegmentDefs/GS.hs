-- |

module X12.Definitions.SegmentDefs.GS where
import X12.Definitions
import X12.Definitions.ElementDefs

gs = SegmentDef { segmentId = "GS"
                , segmentName = "Functional Group Header"
                , segmentPurpose = "To indicate the beginning of a functional group and to provider control information"
                , elementUses = [ SimpleElementUse e479 Mandatory (Bounded 1)
                                , SimpleElementUse e142 Mandatory (Bounded 1)
                                , SimpleElementUse e124 Mandatory (Bounded 1)
                                , SimpleElementUse e373 Mandatory (Bounded 1)
                                , SimpleElementUse e337 Mandatory (Bounded 1)
                                , SimpleElementUse e28  Mandatory (Bounded 1)
                                , SimpleElementUse e455 Mandatory (Bounded 1)
                                , SimpleElementUse e480 Mandatory (Bounded 1)
                                ]
                }
