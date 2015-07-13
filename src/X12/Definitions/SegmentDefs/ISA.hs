{-# LANGUAGE OverloadedStrings #-}
-- | ISA Segment Definition

module X12.Definitions.SegmentDefs.ISA where
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.SegmentDef
import X12.Definitions.ElementDefs
import X12.Definitions.ElementUse

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
