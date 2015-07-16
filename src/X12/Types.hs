{-# LANGUAGE GADTs #-}
-- |

module X12.Types where
import X12.Definitions
import Data.Time.Calendar
import Data.Text

data ID
data AN
data DT

data Value t where
  ID :: Text -> Value ID
  AN :: Text -> Value AN
  DT :: Day -> Value DT

data I01 = I01 (Value ID)

data I02 = I02 (Value AN)

data ISA = ISA I01 I02

data IEA = IEA (Value ID)
