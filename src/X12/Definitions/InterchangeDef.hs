-- |

module X12.Definitions.InterchangeDef where
import X12.Definitions.SegmentUse

data InterchangeDef = InterchangeDef { interchangeDefId :: String
                                     , headerSegmentUses :: [SegmentUse]
                                     , trailerSegmentUsers :: [SegmentUse]
                                     }
                    deriving Show
