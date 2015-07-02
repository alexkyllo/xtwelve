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

i03 = ElementVal { elementId = "I03"
                 , elementName = "Security Information Qualifier"
                 , elementValue = Nothing
                 , elementMinLength = 2
                 , elementMaxLength = 2
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("00", "No Security Information Present (No Meaningful Information in I04)")
                                                      , ("01", "Password")])
                 }

i04 = ElementVal { elementId = "I04"
                 , elementName = "Security Information"
                 , elementValue = Nothing
                 , elementMinLength = 10
                 , elementMaxLength = 10
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i05 = ElementVal { elementId = "I05"
                 , elementName = "Interchange ID Qualifier"
                 , elementValue = Nothing
                 , elementMinLength = 2
                 , elementMaxLength = 2
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("01","Duns (Dun & Bradstreet)")
                                                      , ("02","SCAC (Standard Carrier Alpha Code)")
                                                      , ("ZZ", "Mutually Defined")
                                                      ])
                 }

i06 = ElementVal { elementId = "I06"
                 , elementName = "Interchange Sender ID"
                 , elementValue = Nothing
                 , elementMinLength = 15
                 , elementMaxLength = 15
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i07 = ElementVal { elementId = "I07"
                 , elementName = "Interchange Receiver ID"
                 , elementValue = Nothing
                 , elementMinLength = 15
                 , elementMaxLength = 15
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i08 = ElementVal { elementId = "I08"
                 , elementName = "Interchange Date"
                 , elementValue = Nothing
                 , elementMinLength = 6
                 , elementMaxLength = 6
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i09 = ElementVal { elementId = "I09"
                 , elementName = "Interchange Time"
                 , elementValue = Nothing
                 , elementMinLength = 4
                 , elementMaxLength = 4
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }
