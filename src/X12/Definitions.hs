-- |

module X12.Definitions where

import Data.Text
import Data.Map

data Separators = Separators { componentSeparator :: Char
                             , repetitionSeparator :: Char
                             , elementSeparator :: Char
                             , segmentSeparator :: Char
                             }
                  deriving (Eq, Show)

data RepeatCount = Bounded Int | Unbounded
                 deriving (Eq, Show)

data Requirement = Mandatory | Optional | Relational
                 deriving (Eq, Show)

data ElementDef =
  ElementDef { elementId :: Text
             , elementName :: Text
             , elementType :: Text
             , elementMinLength :: Int
             , elementMaxLength :: Int
             , elementPrecision :: Maybe Int -- Decimal precision for N and R values
             , elementCodeList :: Maybe (Map Text Text) -- Code Lists for ID values
             }
  deriving (Eq, Show)


data ElementUse = ElementUse ElementDef Requirement RepeatCount
                deriving (Eq, Show)

data SegmentDef = SegmentDef { segmentId :: Text
                             , segmentName :: Text
                             , segmentPurpose :: Text
                             , elementUses :: [ElementUse]
                             }
                deriving (Eq, Show)


data SegmentUse = SegmentUse { segmentUseDef :: SegmentDef
                             , segmentReq :: Requirement
                             , segmentRepeatCount :: RepeatCount
                             , segmentParent :: Maybe LoopDef
                             }
                deriving (Eq, Show)

data InterchangeDef = InterchangeDef { interchangeDefId :: Text
                                     , interchangeHeaderSegmentUses :: [SegmentUse]
                                     , interchangeTrailerSegmentUses :: [SegmentUse]
                                     }
                    deriving (Eq, Show)


data FunctionalGroupDef = FunctionalGroupDef { functionalGroupDefId :: Text
                                             , functionalGroupHeaderSegmentUses :: [SegmentUse]
                                             , functionalGroupTrailerSegmentUses :: [SegmentUse]
                                             }


data TransactionSetDef = TransactionSetDef { transactionSetDefId :: Text
                                           , transactionSetName :: Text
                                           , transactionSetFunctionalGroup :: Text
                                           , transactionSetTableDefs :: [TableDef]
                                           }


data TableType = Header | Detail | Summary
               deriving (Eq, Show)

data TableDef = TableDef { tableType :: TableType
                         , tableId :: Text
                         , tableHeaderSegmentUses :: [SegmentUse]
                         , tableTrailerSegmentUses :: [SegmentUse]
                         , tableLoopDefs :: [LoopDef]
                         }
              deriving (Eq, Show)

data LoopDef = LoopDef { loopId :: Text
                       , loopRepeatCount :: RepeatCount
                       }
             deriving (Eq, Show)
