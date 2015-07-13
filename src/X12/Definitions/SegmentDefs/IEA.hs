{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Definitions.SegmentDefs.IEA where
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.SegmentDef
import X12.Definitions.ElementDefs
import X12.Definitions.ElementUse

iea = SegmentDef { segmentId = "IEA"
                 , segmentName = "Interchange Control Trailer"
                 , segmentPurpose = "To define the end of an interchange of zero or more functional groups and interchange-related control segments"
                 , elementUses = [ SimpleElementUse i16 Mandatory (Bounded 1)
                                 ]
                 }
