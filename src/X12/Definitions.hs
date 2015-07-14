-- |

module X12.Definitions where

import Data.Text
import Data.Map

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


data ElementUse = SimpleElementUse ElementDef Requirement RepeatCount
                | ComponentElementUse ElementDef Requirement
                | CompositeElementUse ElementDef Requirement RepeatCount
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


data TransactionSetDef = TransactionSetDef { transactionSetDefId :: Text
                                           , transactionSetName :: Text
                                           , transactionSetFunctionalGroup :: Text
                                           , transactionSetTableDefs :: [TableDef]
                                           }


data TableType = Header | Detail | Summary

data TableDef = TableDef { tableType :: TableType
                         , tableId :: Text
                         , tableHeaderSegmentUses :: [SegmentUse]
                         , tableTrailerSegmentUses :: [SegmentUse]
                         , tableLoopDefs :: [LoopDef]
                         }

data LoopDef = LoopDef { loopId :: Text
                       , loopRepeatCount :: RepeatCount
                       }
             deriving Show
