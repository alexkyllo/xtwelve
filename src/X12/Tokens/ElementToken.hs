-- |

module X12.Tokens.ElementToken where
import Data.Text

data ElementToken = SimpleElementToken Text
                  | ComponentElementToken Text
                  | CompositeElementToken [ElementToken]
                  | RepeatedElementToken [ElementToken]
