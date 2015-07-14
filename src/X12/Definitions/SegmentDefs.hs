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
                 , elementUses = [ ElementUse i01 Mandatory (Bounded 1)
                                 , ElementUse i02 Mandatory (Bounded 1)
                                 , ElementUse i03 Mandatory (Bounded 1)
                                 , ElementUse i04 Mandatory (Bounded 1)
                                 , ElementUse i05 Mandatory (Bounded 1)
                                 , ElementUse i06 Mandatory (Bounded 1)
                                 , ElementUse i05 Mandatory (Bounded 1)
                                 , ElementUse i07 Mandatory (Bounded 1)
                                 , ElementUse i08 Mandatory (Bounded 1)
                                 , ElementUse i09 Mandatory (Bounded 1)
                                 , ElementUse i10 Mandatory (Bounded 1)
                                 , ElementUse i01 Mandatory (Bounded 1)
                                 , ElementUse i11 Mandatory (Bounded 1)
                                 , ElementUse i12 Mandatory (Bounded 1)
                                 , ElementUse i13 Mandatory (Bounded 1)
                                 , ElementUse i14 Mandatory (Bounded 1)
                                 , ElementUse i15 Mandatory (Bounded 1)
                                 ]
                 }

iea = SegmentDef { segmentId = "IEA"
                 , segmentName = "Interchange Control Trailer"
                 , segmentPurpose = "To define the end of an interchange of zero or more functional groups and interchange-related control segments"
                 , elementUses = [ ElementUse i16 Mandatory (Bounded 1)
                                 ]
                 }

gs = SegmentDef { segmentId = "GS"
                , segmentName = "Functional Group Header"
                , segmentPurpose = "To indicate the beginning of a functional group and to provider control information"
                , elementUses = [ ElementUse e479 Mandatory (Bounded 1)
                                , ElementUse e142 Mandatory (Bounded 1)
                                , ElementUse e124 Mandatory (Bounded 1)
                                , ElementUse e373 Mandatory (Bounded 1)
                                , ElementUse e337 Mandatory (Bounded 1)
                                , ElementUse e28  Mandatory (Bounded 1)
                                , ElementUse e455 Mandatory (Bounded 1)
                                , ElementUse e480 Mandatory (Bounded 1)
                                ]
                }

ge = SegmentDef { segmentId = "GE"
                , segmentName = "Functional Group Trailer"
                , segmentPurpose = "To indicate the end of a functional group and to provider control information"
                , elementUses = [ ElementUse e97 Mandatory (Bounded 1)
                                , ElementUse e28 Mandatory (Bounded 1)
                                ]
                }

st = SegmentDef "ST" "Transaction Set Header" "To indicate the start of a transaction set and assign a control number" [ ElementUse e143 Mandatory (Bounded 1)
                                                                                                                       , ElementUse e329 Mandatory (Bounded 1)
                                                                                                                       ]

beg = SegmentDef "BEG" "Beginning Segment for Purchase Order" "To indicate the beginning of the Purchase Order Transaction Set and transmit identifying numbers and dates" [ ElementUse e353 Mandatory (Bounded 1)
                                                                                                                                                                           ]

cur = SegmentDef "CUR" "Currency" "To specify the currency (dollars, pounds, francs, etc) used in a transaction" [ ElementUse e98 Mandatory (Bounded 1)
                                                                                                                 ]
