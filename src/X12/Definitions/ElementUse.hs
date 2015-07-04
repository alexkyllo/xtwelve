-- | Data types for specifying how an Element is used within a Segment.

module X12.Definitions.ElementUse where
import X12.Definitions.ElementDefs (ElementDef(..))
import X12.Definitions.RepeatCount
import X12.Definitions.Requirement


data ElementUse = ElementUse { elementUseDef :: ElementDef
                             , elementReq :: Requirement
                             , elementRepeatCount :: RepeatCount
                             }
                deriving Show
