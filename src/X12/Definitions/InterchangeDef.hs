-- |

module X12.Definitions.InterchangeDef where
import Data.Text
import X12.Definitions.SegmentUse

data InterchangeDef = InterchangeDef { interchangeDefId :: Text
                                     , interchangeHeaderSegmentUses :: [SegmentUse]
                                     , interchangeTrailerSegmentUses :: [SegmentUse]
                                     }
                    deriving Show
