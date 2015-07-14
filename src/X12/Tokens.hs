-- |

module X12.Tokens where

import Data.Text

data ElementToken = SimpleElementToken Text
                  | ComponentElementToken Text
                  | CompositeElementToken [ElementToken]
                  | RepeatedElementToken [ElementToken]
                  deriving (Eq, Show)

data SegmentToken = SegmentToken { segmentTokenId :: Text
                                 , elementTokens :: [ElementToken]
                                 }
                  deriving (Eq, Show)
