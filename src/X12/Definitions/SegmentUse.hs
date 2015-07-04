-- | Data types for specifying how a Segment is used within an Interchange/Loop/Table

module X12.Definitions.SegmentUse where
import X12.Definitions.SegmentDefs (SegmentDef(..))
import X12.Definitions.RepeatCount
import X12.Definitions.Requirement
import X12.Definitions.LoopDef

data SegmentUse = SegmentUse { segmentUseDef :: SegmentDef
                             , segmentReq :: Requirement
                             , segmentRepeatCount :: RepeatCount
                             , segmentParent :: LoopDef
                             }
                deriving Show
