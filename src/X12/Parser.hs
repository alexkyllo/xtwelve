{-# LANGUAGE OverloadedStrings #-}
-- | Parser for ANSI X12 EDI Data Format

module X12.Parser where
import Prelude hiding (concat, takeWhile, take, lookup)
import X12.Parser.Value
import X12.Tokenizer
import Data.Either
import Data.Map hiding (map)
import Data.Text (Text)
import Data.Attoparsec.Text
import Control.Applicative (pure, many, (<*),(*>),(<*>),(<|>),(<$>))

type Element = Text
type Identifier = Text
type FunctionalGroup = [TransactionSet]


parseSegment :: Char -> Char -> Parser [Value]
parseSegment sep term = do
  ident <- element sep term
  typeList <- pure (lookup ident segmentTypes)
  case typeList of
    Nothing -> error "Segment definition not found."
    Just xs -> do
      elements <- sepBy (parseAN sep term) (char sep)
      return elements --infinite loop!

segmentTypes = fromList ([("ISA" :: Text, isaTypes)])

getSegmentParsers :: [Text] -> [Parser Value]
getSegmentParsers xs = fmap value xs

isaTypes :: [Text]
isaTypes = ["ID","ID","AN","ID","AN","ID","AN","ID","AN","DT","TM","ID","ID","N","ID","ID","AN"]

textParser :: Char -> Char -> Parser Text
textParser sepChar termChar = takeWhile1 (`notElem` [sepChar, termChar])

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

data SegmentVal =
  SegmentVal { segmentValId :: Text
             , elementVals :: [ElementVal]
             }
  deriving Show

data Loop =
  Loop { loopId :: Text
       , segments :: [Segment]
       }
  deriving Show

data ElementVal =
  ElementVal { elementId :: Text
             , elementType :: Text
             , elementValue :: Maybe Value
             }
  deriving Show

parseSegmentTokE :: Either String [Text] -> Either [String] [Value]
parseSegmentTokE (Right r) = do
  parsedElements <- fromEithers $ zipWith parseOnly (map value isaTypes) r
  return parsedElements
parseSegmentTokE (Left err) = error $ "A parsing error was found: " ++ err


parseSegmentTok :: [Text] -> [Either String Value]
parseSegmentTok s@(x:xs) = zipWith parseOnly (map value $ getSegmentTypes x) s
parseSegmentTok _ = error "Empty segment."

getSegmentTypes :: Text -> [Text]
getSegmentTypes x = case lookup x segmentTypes of
  Just xs -> xs
  Nothing -> error "Segment definition not found."

fromEithers :: [Either String Value] -> Either [String] [Value]
fromEithers eithers = case lefts eithers of
  [] -> Right (rights eithers)
  _ -> Left (lefts eithers)

parseInterchangeTok :: Either String [[Text]] -> [[Either String Value]]
parseInterchangeTok (Right r) = map parseSegmentTok r
parseInterchangeTok (Left err) = error $ "A parsing error was found: " ++ err
