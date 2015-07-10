-- |

module X12.Definitions.InterchangeDefs.FourOhOne.SegmentDefs where
import Data.Text
import Data.Map hiding (map)
import X12.Definitions.SegmentDef
import X12.Definitions.SegmentDefs.ISA
import X12.Definitions.SegmentDefs.IEA

segmentDefs = fromList([("ISA" :: Text, isa)
                       , ("IEA" :: Text, iea)
                       ])
