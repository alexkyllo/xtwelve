-- |

module X12.Values.TableVal where
import X12.Definitions.TableDef
import X12.Values.LoopVal

data TableVal = TableVal { tableDef :: TableDef
                         , children :: SegmentLoopVal
                         }
