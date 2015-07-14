-- |

module X12.Values where
import X12.Definitions

data Position = Position { offset :: Int
                          , line :: Int
                          , column :: Int
                        --, pathname :: Text
                         }

data ElementVal = ElementVal ElementUse
                deriving Show


data SegmentVal = SegmentDef [ElementVal]
                deriving Show


data SegmentLoopVal = SegmentVal SegmentDef [ElementVal]
                    | LoopVal LoopDef SegmentLoopVal
                    deriving Show

data TableVal = TableVal TableDef SegmentLoopVal
              deriving Show

data InterchangeVal = InterchangeVal InterchangeDef [SegmentVal] Separators
                    deriving Show
