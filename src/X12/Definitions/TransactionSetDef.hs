-- |

module X12.Definitions.TransactionSetDef where
import Data.Text
import X12.Definitions.TableDef

data TransactionSetDef = TransactionSetDef { transactionSetDefId :: Text
                                           , transactionSetName :: Text
                                           , transactionSetFunctionalGroup :: Text
                                           , transactionSetTableDefs :: [TableDef]
                                           }
