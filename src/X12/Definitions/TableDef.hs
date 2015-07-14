-- |

module X12.Definitions.TableDef where
import Data.Text
import X12.Definitions.SegmentUse
import X12.Definitions.LoopDef

data TableType = Header | Detail | Summary

data TableDef = TableDef { tableType :: TableType
                         , tableId :: Text
                         , tableHeaderSegmentUses :: [SegmentUse]
                         , tableTrailerSegmentUses :: [SegmentUse]
                         , tableLoopDefs :: [LoopDef]
                         }
