-- |

module X12.Definitions.TransactionSetDefs.PO850 where
import X12.Definitions.TransactionSetDef
import X12.Definitions.SegmentUse
import X12.Definitions.SegmentDefs
import X12.Definitions.TableDef
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount

po850 = TransactionSetDef "PO" "850" "Purchase Order" [poHdrTable, poDtlTable, poSummaryTable]

poHdrTable = TableDef Header "Table 1 - Header" [ SegmentUse st Mandatory (Bounded 1) Nothing
                                                , SegmentUse beg Mandatory (Bounded 1) Nothing
                                             -- , SegmentUse cur Optional (Bounded 1)
                                             -- , SegmentUse ref Optional Unbounded
                                             -- , SegmentUse per Optional (Bounded 3)
                                             -- , SegmentUse dtm Optional (Bounded 10)
                                                ] [] []

poDtlTable = TableDef Detail "Table 2 - Detail" [] [] []

poSummaryTable = TableDef Detail "Table 3 - Summary" [] [] []
