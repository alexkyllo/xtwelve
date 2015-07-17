-- |

module X12.Builder where

import X12.Parser.Value
import Data.Text (Text(..), pack)
import Data.Time.Calendar (Day(..), fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Format

isa :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Day ->
       TimeOfDay -> Text -> Text -> Integer -> Text -> Text -> [Value]
isa isa01 isa02 isa03 isa04 isa05 isa06 isa07 isa08 isa09 isa10 isa11 isa12 isa13 isa14 isa15 = [(ID "ISA"), (ID isa01), (AN isa02), (ID isa03), (AN isa04), (ID isa05), (AN isa06), (ID isa07), (AN isa08), (DT isa09), (TM isa10), (ID isa11), (ID isa12), (N isa13), (ID isa14), (ID isa15), (AN ">")]

gs :: Text -> Text -> Text -> Day -> TimeOfDay -> Integer -> Text -> Text -> [Value]
gs gs01 gs02 gs03 gs04 gs05 gs06 gs07 gs08 = [(ID "GS"), (ID gs01), (AN gs02), (AN gs03), (DT gs04), (TM gs05), (N gs06), (ID gs07), (AN gs08)]
