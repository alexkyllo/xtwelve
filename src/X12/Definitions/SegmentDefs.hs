-- |

module X12.Definitions.SegmentDefs where
import X12.Definitions.SegmentDef
import X12.Definitions.SegmentDefs.GS
import X12.Definitions.ElementUse
import X12.Definitions.ElementDefs
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import Data.Map hiding (map)
import Data.Text

segmentDefs = fromList [( "GS" :: Text, gs)
                       ]

st = SegmentDef "ST" "Transaction Set Header" "To indicate the start of a transaction set and assign a control number" [ SimpleElementUse e143 Mandatory (Bounded 1)
                                                                                                                       , SimpleElementUse e329 Mandatory (Bounded 1)
                                                                                                                       ]

beg = SegmentDef "BEG" "Beginning Segment for Purchase Order" "To indicate the beginning of the Purchase Order Transaction Set and transmit identifying numbers and dates" [
                                                                                                                                                                           ]
