-- |

module X12.Definitions.FunctionalGroupDef where
import Data.Text
import X12.Definitions.SegmentUse

data FunctionalGroupDef = FunctionalGroupDef { functionalGroupDefId :: Text
                                             , functionalGroupHeaderSegmentUses :: [SegmentUse]
                                             , functionalGroupTrailerSegmentUses :: [SegmentUse]
                                             }
