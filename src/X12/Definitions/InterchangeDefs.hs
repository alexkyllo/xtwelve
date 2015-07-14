-- |

module X12.Definitions.InterchangeDefs where
import X12.Definitions
import X12.Definitions.SegmentDefs
import Data.Map hiding (Map)
import Data.Text

i00401 = InterchangeDef { interchangeDefId = "00401"
                        , interchangeHeaderSegmentUses = [isaUse]
                        , interchangeTrailerSegmentUses = [ieaUse]
                        }

isaUse = SegmentUse isa Mandatory (Bounded 1) Nothing

ieaUse = SegmentUse iea Mandatory (Bounded 1) Nothing


segmentDict = fromList [ ("ISA" :: Text, isa)
                       , ("IEA" :: Text, iea)
                       ]

interchangeDict = fromList [ ("00401" :: Text, i00401)
                           ]
