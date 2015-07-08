-- |

module X12.Separators where

data Separators = Separators { componentSeparator :: Char
                             , repetitionSeparator :: Char
                             , elementSeparator :: Char
                             , segmentSeparator :: Char
                             }
                  deriving Show
