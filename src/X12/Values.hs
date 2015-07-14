-- |

module X12.Values where
import X12.Definitions

data Position = Position { offset :: Int
                          , line :: Int
                          , column :: Int
                        --, pathname :: Text
                         }
              deriving (Eq, Show)

data ElementVal = ElementVal ElementUse
                deriving (Eq, Show)


data SegmentVal = SegmentDef [ElementVal]
                deriving (Eq, Show)


data SegmentLoopVal = SegmentVal SegmentDef [ElementVal]
                    | LoopVal LoopDef SegmentLoopVal
                    deriving (Eq, Show)

data TableVal = TableVal TableDef SegmentLoopVal
              deriving (Eq, Show)

data InterchangeVal = InterchangeVal InterchangeDef [SegmentVal] Separators
                    deriving (Eq, Show)

data TransmissionVal = TransmissionVal [InterchangeVal]
                     deriving (Eq, Show)
