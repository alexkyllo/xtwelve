{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Definitions.SegmentDefs.IEA where
import X12.Definitions
import X12.Definitions.ElementDefs

iea = SegmentDef { segmentId = "IEA"
                 , segmentName = "Interchange Control Trailer"
                 , segmentPurpose = "To define the end of an interchange of zero or more functional groups and interchange-related control segments"
                 , elementUses = [ SimpleElementUse i16 Mandatory (Bounded 1)
                                 ]
                 }
