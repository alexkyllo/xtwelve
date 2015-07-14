-- |

module X12.Definitions.InterchangeDefs.FourOhOne.SegmentDefs where
import Data.Text
import Data.Map hiding (map)
import X12.Definitions
import X12.Definitions.SegmentDefs.ISA
import X12.Definitions.SegmentDefs.IEA

segmentDict = fromList [("ISA" :: Text, isa)
                       , ("IEA" :: Text, iea)
                       ]
