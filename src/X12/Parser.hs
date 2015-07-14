-- | Parser for ANSI X12 EDI Data Format

module X12.Parser where
import Prelude hiding (concat, takeWhile, take, lookup)
import X12.Parser.Value
import X12.Tokenizer
import X12.Tokens
import X12.Values
import X12.Definitions
import X12.Definitions.ElementDefs
import X12.Definitions.SegmentDefs
import X12.Definitions.InterchangeDefs
import Data.Either
import Data.Map hiding (map)
import Data.Text (Text, unpack, append)
import Data.Attoparsec.Text
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Scientific (Scientific)

import Control.Applicative (pure, many, (<*),(*>),(<*>),(<|>),(<$>))

readInterchange :: Either String ([SegmentTok], Separators) -> Either String InterchangeVal
readInterchange (Right (segments, seps)) = case iDef of
                                            (Just def) -> Right $ InterchangeVal def [] seps
                                            Nothing -> Left $ "Interchange definition not found for version" ++ unpack version
  where iDef = lookup version interchangeDict
        version = head segments !! 12

readInterchange (Left err) =  error $ "A parsing error was found: " ++ err
