{-# LANGUAGE OverloadedStrings #-}
-- | Parser for ANSI X12 EDI Data Format

module X12.Parser where
import Prelude hiding (concat, takeWhile, take, lookup)
import X12.Parser.Value
import X12.Tokenizer
import X12.Separators
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.ElementDefs
import X12.Definitions.SegmentDefs
import qualified X12.Definitions.SegmentDefs.ISA
import qualified X12.Definitions.SegmentDefs.IEA
import X12.Definitions.SegmentUse
import X12.Definitions.InterchangeDef
import X12.Values.InterchangeVal
import X12.Values.SegmentVal
import Data.Either
import Data.Map hiding (map)
import Data.Text (Text, unpack)
import Data.Attoparsec.Text
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Scientific (Scientific)
import Control.Applicative (pure, many, (<*),(*>),(<*>),(<|>),(<$>))

type Element = Text
type Identifier = Text
type FunctionalGroup = [TransactionSet]

segmentTypes = fromList ([("ISA" :: Text, isaTypes)
                         , ("GS" :: Text, gsTypes)
                         , ("ST" :: Text, stTypes)])

isaTypes :: [Text]
isaTypes = ["ID","ID","AN","ID","AN","ID","AN","ID","AN","DT","TM","ID","ID","N","ID","ID","AN"]

gsTypes = ["ID","ID","AN","AN","DT","TM","N","ID","AN"]

stTypes :: [Text]
stTypes = ["ID","ID","ID","ID","ID","AN","AN","DT","ID","ID","R","ID","ID","AN","AN","AN","ID","ID","R","DT","N","DT","N","N","DT","N","R","AN"]

data Interchange =
  Interchange { interchangeSegment :: Segment
              , functionalGroups :: [FunctionalGroup]
              }
  deriving Show

data TransactionSet =
  TransactionSet { transactionSetId :: Text
                 , tables :: [Table]
                 }
  deriving Show

data TableType = Header | Detail | Summary
               deriving Show

data Table =
  Table { tableType :: TableType
        , tableLoops :: [Loop]
        , tableSegments :: [Segment]
        }
  deriving Show

data Segment =
  Segment { segmentId :: Text
          , elements :: [Text]
          }
  deriving Show

data Loop =
  Loop { loopId :: Text
       , segments :: [Segment]
       }
  deriving Show

parseSegmentTokE :: Either String [Text] -> Either String [Either String Value]
parseSegmentTokE (Right s@(x:xs)) = case getSegmentTypes x of
  Just segTypes -> Right $ zipWith parseOnly (map value segTypes) s
  Nothing -> Left $ "Segment definition not found: " ++ (unpack x)
parseSegmentTokE _ = Left "Received an empty segment."

parseSegmentTok :: [Text] -> Either String [Either String Value]
parseSegmentTok s@(x:xs) = case getSegmentTypes x of
  Just segTypes -> Right $ zipWith parseOnly (map value segTypes) s
  Nothing -> Left $ "Segment definition not found: " ++ (unpack x)
parseSegmentTok _ = Left "Received an empty segment."

getSegmentTypes :: Text -> Maybe [Text]
getSegmentTypes x = lookup x segmentTypes

fromEithers :: [Either String Value] -> Either [String] [Value]
fromEithers eithers = case lefts eithers of
  [] -> Right (rights eithers)
  _ -> Left (lefts eithers)

parseInterchangeTok :: Either String [[Text]] -> [Either String [Either String Value]]
parseInterchangeTok (Right r) = map parseSegmentTok r
parseInterchangeTok (Left err) = error $ "A parsing error was found: " ++ err

readInterchange :: Either String ([SegmentToken], Separators) -> InterchangeVal
readInterchange (Right (segments, seps)) = InterchangeVal { interchangeDef = iDef
                                                         , children = []
                                                         , separators = seps
                                                         }
  where iDef = InterchangeDef { interchangeDefId = segments !! 0 !! 12
                                        , headerSegmentUses = [SegmentUse { segmentUseDef = X12.Definitions.SegmentDefs.ISA.isa
                                                                          , segmentReq = Mandatory
                                                                          , segmentRepeatCount = Bounded 1
                                                                          , segmentParent = Nothing
                                                                          }
                                                              ]
                                        , trailerSegmentUses = [ SegmentUse { segmentUseDef = X12.Definitions.SegmentDefs.IEA.iea
                                                                            , segmentReq = Mandatory
                                                                            , segmentRepeatCount = Bounded 1
                                                                            , segmentParent = Nothing
                                                                            }
                                                               ]
                                        }
readInterchange (Left err) =  error $ "A parsing error was found: " ++ err
