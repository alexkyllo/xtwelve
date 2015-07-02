{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Definitions.ElementTypes where
import Data.Map hiding (map)
import Data.Text
import X12.Parser.Value

data ElementVal =
  ElementVal { elementId :: Text
             , elementName :: Text
             , elementValue :: Maybe Value
             , elementMinLength :: Int
             , elementMaxLength :: Int
             , elementPrecision :: Maybe Int --for N and R values
             , elementCodeList :: (Maybe (Map Text Text))
             }
  deriving Show

i01 = ElementVal { elementId = "I01"
                 , elementName = "Authorization Information Qualifier"
                 , elementValue = Nothing
                 , elementMinLength = 2
                 , elementMaxLength = 2
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("00", "No Authorization Information Present (No Meaningful Information in I02)")])
                 }

i02 = ElementVal { elementId = "I02"
                 , elementName = "Authorization Information"
                 , elementValue = Nothing
                 , elementMinLength = 10
                 , elementMaxLength = 10
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }
