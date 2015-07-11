-- |

module X12.Definitions.SegmentDefs where
import X12.Definitions.SegmentDef
import X12.Definitions.SegmentDefs.GS
import Data.Map hiding (map)
import Data.Text

segmentDefs = fromList [( "GS" :: Text, gs)
                       ]
