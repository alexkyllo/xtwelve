-- |

module X12.Definitions.SegmentDefs where
import X12.Definitions
import X12.Definitions.ElementDefs
import Data.Map hiding (map)
import Data.Text

segmentDefs = fromList [( "GS" :: Text, gs)
                       ]


isa = SegmentDef { segmentId = "ISA"
                 , segmentName = "Interchange Control Header"
                 , segmentPurpose = "To start and identify an interchange of zero or more functional groups and interchange-related control segments"
                 , elementUses = [ SimpleElementUse i01 Mandatory (Bounded 1)
                                 , SimpleElementUse i02 Mandatory (Bounded 1)
                                 , SimpleElementUse i03 Mandatory (Bounded 1)
                                 , SimpleElementUse i04 Mandatory (Bounded 1)
                                 , SimpleElementUse i05 Mandatory (Bounded 1)
                                 , SimpleElementUse i06 Mandatory (Bounded 1)
                                 , SimpleElementUse i05 Mandatory (Bounded 1)
                                 , SimpleElementUse i07 Mandatory (Bounded 1)
                                 , SimpleElementUse i08 Mandatory (Bounded 1)
                                 , SimpleElementUse i09 Mandatory (Bounded 1)
                                 , SimpleElementUse i10 Mandatory (Bounded 1)
                                 , SimpleElementUse i01 Mandatory (Bounded 1)
                                 , SimpleElementUse i11 Mandatory (Bounded 1)
                                 , SimpleElementUse i12 Mandatory (Bounded 1)
                                 , SimpleElementUse i13 Mandatory (Bounded 1)
                                 , SimpleElementUse i14 Mandatory (Bounded 1)
                                 , SimpleElementUse i15 Mandatory (Bounded 1)
                                 ]
                 }

iea = SegmentDef { segmentId = "IEA"
                 , segmentName = "Interchange Control Trailer"
                 , segmentPurpose = "To define the end of an interchange of zero or more functional groups and interchange-related control segments"
                 , elementUses = [ SimpleElementUse i16 Mandatory (Bounded 1)
                                 ]
                 }

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

ge = SegmentDef { segmentId = "GE"
                , segmentName = "Functional Group Trailer"
                , segmentPurpose = "To indicate the end of a functional group and to provider control information"
                , elementUses = [ SimpleElementUse e97 Mandatory (Bounded 1)
                                , SimpleElementUse e28 Mandatory (Bounded 1)
                                ]
                }

st = SegmentDef "ST" "Transaction Set Header" "To indicate the start of a transaction set and assign a control number" [ SimpleElementUse e143 Mandatory (Bounded 1)
                                                                                                                       , SimpleElementUse e329 Mandatory (Bounded 1)
                                                                                                                       ]

beg = SegmentDef "BEG" "Beginning Segment for Purchase Order" "To indicate the beginning of the Purchase Order Transaction Set and transmit identifying numbers and dates" [
                                                                                                                                                                           ]
