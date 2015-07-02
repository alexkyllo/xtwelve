-- |

module X12.Definitions.ElementTypes where
import Data.Map hiding (map)
import Data.Text

data ID = ID { elementId :: Text
             , name :: Text
             , minLength :: Int
             , maxLength :: Int
             , codeList :: Map Text Text
             , description :: Text
             }
