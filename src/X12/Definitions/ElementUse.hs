-- | Data types for specifying how an Element is used within a Segment.

module X12.Definitions.ElementUse where
import X12.Definitions.ElementDefs (ElementDef(..))

data ElementReq = Mandatory | Optional | Relational

data ElementRepeatCount = Bounded Int | Unbounded

data ElementUse = ElementUse { elementUseDef :: ElementDef
                             , elementReq :: ElementReq
                             , elementRepeatCount :: ElementRepeatCount
                             }
