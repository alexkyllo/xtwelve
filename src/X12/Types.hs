{-# LANGUAGE GADTs #-}
-- |

module X12.Types where
import X12.Definitions
import X12.Values
import Data.Time.Calendar
import Data.Text

data ID
data AN
data DT
data ISA

data SegmentValue t where
  ISA :: (Value ID) -> (Value AN) -> SegmentValue ISA

data Value t where
  ID :: Text -> Value ID
  AN :: Text -> Value AN
  DT :: Day -> Value DT

data I01 = I01 (Value ID)

data I02 = I02 (Value AN)

--data ISA = ISA I01 I02

data IEA = IEA (Value ID)

-- How to define types for DSL to generate a message?
-- Write a function to generate an ISA SegmentVal from arguments
-- isa seps isa01 isa02 isa03 isa04 isa05 isa06 isa07 isa08 isa09 isa10 isa11 isa12 isa13 isa14 isa15
