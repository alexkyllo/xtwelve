{-# LANGUAGE OverloadedStrings #-}
-- |

module X12.Definitions.ElementDefs where
import Data.Map hiding (map)
import Data.Text
import X12.Parser.Value

data ElementDef =
  ElementDef { elementId :: Text
             , elementName :: Text
             , elementType :: Text
             , elementMinLength :: Int
             , elementMaxLength :: Int
             , elementPrecision :: Maybe Int -- Decimal precision for N and R values
             , elementCodeList :: Maybe (Map Text Text) -- Code Lists for ID values
             }
  deriving Show

e479 = ElementDef "E479" "Functional Identifier Code" "ID" 2 2 Nothing (Just e479CodeList)

e479CodeList = fromList [ ( "PO"
                          , "Purchase Order (850)"
                          )
                        ]

e142 = ElementDef "E142" "Application's Sender Code" "ID" 2 15 Nothing Nothing

e124 = ElementDef "E124" "Aplication's Receiver Code" "ID" 2 15 Nothing Nothing

e373 = ElementDef "E373" "Date" "DT" 8 8 Nothing Nothing

e337 = ElementDef "E337" "Time" "TM" 4 4 Nothing Nothing

e28 = ElementDef "E28" "Group Control Number" "N" 1 9 (Just 0) Nothing

e455 = ElementDef "E455" "Responsible Agency Code" "ID" 1 2 Nothing Nothing

e480 = ElementDef "E480" "Version / Release / Identifier Code" "AN" 1 12 Nothing (Just e480CodeList)

e480CodeList = fromList [ ("004010X091","Draft Standards Approved for Publication by ASC X12 Procedures Review Board through October 1997")
                        , ("004010VICS","Voluntary Industry Commerce Standards v 004010")
                        ]

e97 = ElementDef "E97" "Number of Transaction Sets Included" "N" 1 6 (Just 0) Nothing

i01 = ElementDef { elementId = "I01"
                 , elementName = "Authorization Information Qualifier"
                 , elementType = "ID"
                 , elementMinLength = 2
                 , elementMaxLength = 2
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList [("00", "No Authorization Information Present (No Meaningful Information in I02)")]
                 }

i02 = ElementDef { elementId = "I02"
                 , elementName = "Authorization Information"
                 , elementType = "AN"
                 , elementMinLength = 10
                 , elementMaxLength = 10
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i03 = ElementDef { elementId = "I03"
                 , elementName = "Security Information Qualifier"
                 , elementType = "ID"
                 , elementMinLength = 2
                 , elementMaxLength = 2
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("00", "No Security Information Present (No Meaningful Information in I04)")
                                                      , ("01", "Password")])
                 }

i04 = ElementDef { elementId = "I04"
                 , elementName = "Security Information"
                 , elementType = "AN"
                 , elementMinLength = 10
                 , elementMaxLength = 10
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i05 = ElementDef { elementId = "I05"
                 , elementName = "Interchange ID Qualifier"
                 , elementType = "ID"
                 , elementMinLength = 2
                 , elementMaxLength = 2
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("01","Duns (Dun & Bradstreet)")
                                                      , ("02","SCAC (Standard Carrier Alpha Code)")
                                                      , ("ZZ", "Mutually Defined")
                                                      ])
                 }

i06 = ElementDef { elementId = "I06"
                 , elementName = "Interchange Sender ID"
                 , elementType = "AN"
                 , elementMinLength = 15
                 , elementMaxLength = 15
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i07 = ElementDef { elementId = "I07"
                 , elementName = "Interchange Receiver ID"
                 , elementType = "AN"
                 , elementMinLength = 15
                 , elementMaxLength = 15
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i08 = ElementDef { elementId = "I08"
                 , elementName = "Interchange Date"
                 , elementType = "DT"
                 , elementMinLength = 6
                 , elementMaxLength = 6
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i09 = ElementDef { elementId = "I09"
                 , elementName = "Interchange Time"
                 , elementType = "TM"
                 , elementMinLength = 4
                 , elementMaxLength = 4
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i10 = ElementDef { elementId = "I10"
                 , elementName = "Interchange Control Standards Identifier"
                 , elementType = "ID"
                 , elementMinLength = 1
                 , elementMaxLength = 1
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("U","U.S. EDI Community of ASC X12, TDCC, and UCS")])
                 }

i11 = ElementDef { elementId = "I11"
                 , elementName = "Interchange Control Version Number"
                 , elementType = "ID"
                 , elementMinLength = 5
                 , elementMaxLength = 5
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("00401","Draft Standards for Trial Use Approved for Publication by ASC X12 Procedures Review Board through October 1997")])
                 }

i12 = ElementDef { elementId = "I12"
                 , elementName = "Interchange Control Number"
                 , elementType = "AN"
                 , elementMinLength = 9
                 , elementMaxLength = 9
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i13 = ElementDef { elementId = "I13"
                 , elementName = "Acknowlegment Requested"
                 , elementType = "ID"
                 , elementMinLength = 1
                 , elementMaxLength = 1
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("0","No Interchange Acknowledgment Requested")
                                                      , ("1", "Interchange Acknowledgment Requested (TA1)")
                                                      ])
                 }

i14 = ElementDef { elementId = "I14"
                 , elementName = "Interchange Usage Indicator"
                 , elementType = "ID"
                 , elementMinLength = 1
                 , elementMaxLength = 1
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("I","Information")
                                                     , ("P", "Production Data")
                                                     , ("T", "Test Data")
                                                     ])
                 }

i15 = ElementDef { elementId = "I15"
                 , elementName = "Component Element Separator"
                 , elementType = "AN"
                 , elementMinLength = 1
                 , elementMaxLength = 1
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i16 = ElementDef { elementId = "I16"
                 , elementName = "Number of Included Functional Groups"
                 , elementType = "N"
                 , elementMinLength = 1
                 , elementMaxLength = 5
                 , elementPrecision = Nothing
                 , elementCodeList = Nothing
                 }

i17 = ElementDef { elementId = "I17"
                 , elementName = "Interchange Acknowledgment Code"
                 , elementType = "ID"
                 , elementMinLength = 1
                 , elementMaxLength = 1
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([("A","The Transmitted Interchange Control Structure Header and Trailer Have Been Received and Have No Errors")
                                                     , ("E", "The Transmitted Interchange Control Structure Header and Trailer Have Been Received and Are Accepted But Errors Are Noted. This Means the Sender Must Not Resend the Data.")
                                                     , ("R", "The Transmitted Interchange Control Structure Header and Trailer are Rejected Because of Errors")
                                                     ])
                 }

i18 = ElementDef { elementId = "I18"
                 , elementName = "Interchange Note Code"
                 , elementType = "ID"
                 , elementMinLength = 3
                 , elementMaxLength = 3
                 , elementPrecision = Nothing
                 , elementCodeList = Just $ fromList ([ ("000" , "No error")
                                                      , ("001" , "The Interchange Control Number in the Header and Trailer Do Not Match. The Value From the Header is Used in the Acknowledgement")
                                                      , ("002" , "This Standard as Noted in the Control Standards Identifier is Not Supported")
                                                      , ("003" , "This Version of the Controls is Not Supported")
                                                      , ("004" , "The Segment Terminator is Invalid")
                                                      , ("005" , "Invalid Interchange ID Qualifier for Sender")
                                                      , ("006" , "Invalid Interchange Sender ID")
                                                      , ("007" , "Invalid Interchange ID Qualifier for Receiver")
                                                      , ("008" , "Invalid Interchange Receiver ID")
                                                      , ("009" , "Unknown Interchange Receiver ID")
                                                      , ("010" , "Invalid Authorization Information Qualifier Value")
                                                      , ("011" , "Invalid Authorization Information Value")
                                                      , ("012" , "Invalid Security Information Qualifier Value")
                                                      , ("013" , "Invalid Security Information Value")
                                                      , ("014" , "Invalid Interchange Date Value")
                                                      , ("015" , "Invalid Interchange Time Value")
                                                      , ("016" , "Invalid Interchange Standards Identifier Value")
                                                      , ("017" , "Invalid Interchange Version ID Value")
                                                      , ("018" , "Invalid Interchange Control Number Value")
                                                      , ("019" , "Invalid Acknowledgement Requested Value")
                                                      , ("020" , "Invalid Test Indicator Value")
                                                      , ("021" , "Invalid Number of Included Groups Value")
                                                      , ("022" , "Invalid Control Characters")
                                                      , ("023" , "Improper (Premature) End-of-File (Transmission)")
                                                      , ("024" , "Invalid Interchange Content (ex Invalid GS Segment)")
                                                      , ("025" , "Duplicate Interchange Control Numbers")
                                                      , ("026" , "Invalid Data Structure Separator")
                                                      , ("027" , "Invalid Component Element Separator")
                                                      , ("028" , "Invalid Date in Deferred Delivery Request")
                                                      , ("029" , "Invalid Time in Deferred Delivery Request")
                                                      , ("030" , "Invalid Delivery Time Code in Deferred Delivery Request")
                                                      , ("031" , "Invalid Grade of Service")
                                                      ])
                 }
