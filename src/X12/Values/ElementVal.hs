-- |

module X12.Values.ElementVal where
import X12.Definitions.ElementUse

data Position = Position { offset :: Int
                          , line :: Int
                          , column :: Int
                        --, pathname :: Text
                         }

data ElementVal =
  ElementVal { elementUsage :: ElementUse
             }
  deriving Show
