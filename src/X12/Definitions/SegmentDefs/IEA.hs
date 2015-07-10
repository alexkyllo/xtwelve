{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Definitions.SegmentDefs.IEA where
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.SegmentDef
import X12.Definitions.ElementDefs
import X12.Definitions.ElementUse

iea = SegmentDef { segmentId = "IEA"
                 , elementUses = [ ElementUse { elementUseDef = i16
                                               , elementReq = Mandatory
                                               , elementRepeatCount = Bounded 1
                                               }
                                 ]
                 }
