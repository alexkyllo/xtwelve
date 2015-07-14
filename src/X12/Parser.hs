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
import Data.Either
import Data.Map hiding (map)
import Data.Text (Text, unpack)
import Data.Attoparsec.Text
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Scientific (Scientific)
import Control.Applicative (pure, many, (<*),(*>),(<*>),(<|>),(<$>))

readInterchange :: Either String ([SegmentTok], Separators) -> InterchangeVal
readInterchange (Right (segments, seps)) = InterchangeVal iDef [] seps
  where iDef = InterchangeDef { interchangeDefId = head segments !! 12
                                        , interchangeHeaderSegmentUses = [SegmentUse { segmentUseDef = isa
                                                                          , segmentReq = Mandatory
                                                                          , segmentRepeatCount = Bounded 1
                                                                          , segmentParent = Nothing
                                                                          }
                                                              ]
                                        , interchangeTrailerSegmentUses = [ SegmentUse { segmentUseDef = iea
                                                                            , segmentReq = Mandatory
                                                                            , segmentRepeatCount = Bounded 1
                                                                            , segmentParent = Nothing
                                                                            }
                                                               ]
                                        }
readInterchange (Left err) =  error $ "A parsing error was found: " ++ err
