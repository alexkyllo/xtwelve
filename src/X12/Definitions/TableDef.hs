-- |

module X12.Definitions.TableDef where
import Data.Text
import X12.Definitions.SegmentUse
import X12.Definitions.LoopDef

data TableDef = TableDef { tableId :: Text
                         , tableHeaderSegmentUses :: [SegmentUse]
                         , tableTrailerSegmentUses :: [SegmentUse]
                         , tableLoopDefs :: [LoopDef]
                         , position :: Int
                         }
