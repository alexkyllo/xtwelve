-- |

module X12.Definitions.FunctionalGroupDefs.FortyTen where
import X12.Definitions.SegmentUse
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.SegmentDefs.GS
import X12.Definitions.SegmentDefs.GE
import X12.Definitions.FunctionalGroupDef

fortyTen = FunctionalGroupDef "00401" [gsUse] [geUse]

gsUse = SegmentUse gs Mandatory (Bounded 1) Nothing
geUse = SegmentUse ge Mandatory (Bounded 1) Nothing
