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
                 deriving Show

data Requirement = Mandatory | Optional | Relational
                 deriving Show

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


data ElementUse = ElementUse ElementDef Requirement RepeatCount
                deriving Show

data SegmentDef = SegmentDef { segmentId :: Text
                             , segmentName :: Text
                             , segmentPurpose :: Text
                             , elementUses :: [ElementUse]
                             }
                deriving Show


data SegmentUse = SegmentUse { segmentUseDef :: SegmentDef
                             , segmentReq :: Requirement
                             , segmentRepeatCount :: RepeatCount
                             , segmentParent :: Maybe LoopDef
                             }
                deriving Show

data InterchangeDef = InterchangeDef { interchangeDefId :: Text
                                     , interchangeHeaderSegmentUses :: [SegmentUse]
                                     , interchangeTrailerSegmentUses :: [SegmentUse]
                                     }
                    deriving Show


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
               deriving Show

data TableDef = TableDef { tableType :: TableType
                         , tableId :: Text
                         , tableHeaderSegmentUses :: [SegmentUse]
                         , tableTrailerSegmentUses :: [SegmentUse]
                         , tableLoopDefs :: [LoopDef]
                         }
              deriving Show

data LoopDef = LoopDef { loopId :: Text
                       , loopRepeatCount :: RepeatCount
                       }
             deriving Show
