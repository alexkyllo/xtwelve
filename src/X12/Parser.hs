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

parseISA :: Parser [Value]
parseISA = do
  isa <- take 3
  sep <- anyChar
  isa01 <- parseID sep eol
  char sep
  isa02 <- parseAN sep eol
  char sep
  isa03 <- parseID sep eol
  char sep
  isa04 <- parseAN sep eol
  char sep
  isa05 <- parseID sep eol
  char sep
  isa06 <- parseAN sep eol
  char sep
  isa07 <- parseID sep eol
  char sep
  isa08 <- parseAN sep eol
  char sep
  isa09 <- parseDT
  char sep
  isa10 <- parseTM
  char sep
  isa11 <- parseID sep eol
  char sep
  isa12 <- parseID sep eol
  char sep
  isa13 <- parseN
  char sep
  isa14 <- parseID sep eol
  char sep
  isa15 <- parseID sep eol
  char sep
  delim <- take 1
  term <- take 1
  return $ [ID isa, isa01, isa02, isa03, isa04, isa05, isa06, isa07, isa08, isa09, isa10, isa11, isa12, isa13, isa14, isa15, ID delim, ID term]
  where eol = '\n'

combineParsers :: [Parser a] -> Parser [a]
combineParsers = many . choice

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

isadefaults = SegmentVal { segmentValId="ISA"
                 , elementVals=[
                   ElementVal { elementId="ISA01"
                              , elementType="ID"
                              , elementValue=Just (ID "01")
                              }
                   , ElementVal { elementId="ISA02"
                                , elementType="AN"
                                , elementValue=Just (AN "0000000000")
                                }
                   , ElementVal { elementId="ISA03"
                                , elementType="ID"
                                , elementValue=Just (ID "01")
                                }
                   , ElementVal { elementId="ISA04"
                                , elementType="AN"
                                , elementValue=Just (AN "          ")
                                }
                   , ElementVal { elementId="ISA05"
                                , elementType="ID"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA06"
                                , elementType="AN"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA07"
                                , elementType="ID"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA08"
                                , elementType="AN"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA09"
                                , elementType="DT"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA10"
                                , elementType="TM"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA11"
                                , elementType="ID"
                                , elementValue=Just (ID "U")
                                }
                   , ElementVal { elementId="ISA12"
                                , elementType="ID"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA13"
                                , elementType="N"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA14"
                                , elementType="ID"
                                , elementValue=Nothing
                                }
                   , ElementVal { elementId="ISA15"
                                , elementType="ID"
                                , elementValue=Nothing
                                }
                   ]
                 }

data S850 = S850 {
             }
  deriving Show

testInterchange :: Text
testInterchange = "ISA*01*0000000000*01*ABCCO     *12*4405197800     *01*999999999      *101127*1719*U*00400*000003438*0*P*>\nGS*PO*4405197800*999999999*20101127*1719*1421*X*004010VICS\nST*850*000000010\nBEG*00*SA*08292233294**20101127*610385385\nREF*DP*038\nREF*PS*R\nITD*14*3*2**45**46\nDTM*002*20101214\nPKG*F*68***PALLETIZE SHIPMENT\nPKG*F*66***REGULAR\nTD5*A*92*P3**SEE XYZ RETAIL ROUTING GUIDE\nN1*ST*XYZ RETAIL*9*0003947268292\nN3*31875 SOLON RD\nN4*SOLON*OH*44139\nPO1*1*120*EA*9.25*TE*CB*065322-117*PR*RO*VN*AB3542\nPID*F****SMALL WIDGET\nPO4*4*4*EA*PLT94**3*LR*15*CT\nPO1*2*220*EA*13.79*TE*CB*066850-116*PR*RO*VN*RD5322\nPID*F****MEDIUM WIDGET\nPO4*2*2*EA\nPO1*3*126*EA*10.99*TE*CB*060733-110*PR*RO*VN*XY5266\nPID*F****LARGE WIDGET\nPO4*6*1*EA*PLT94**3*LR*12*CT\nPO1*4*76*EA*4.35*TE*CB*065308-116*PR*RO*VN*VX2332\nPID*F****NANO WIDGET\nPO4*4*4*EA*PLT94**6*LR*19*CT\nPO1*5*72*EA*7.5*TE*CB*065374-118*PR*RO*VN*RV0524\nPID*F****BLUE WIDGET\nPO4*4*4*EA\nPO1*6*696*EA*9.55*TE*CB*067504-118*PR*RO*VN*DX1875\nPID*F****ORANGE WIDGET\nPO4*6*6*EA*PLT94**3*LR*10*CT\nCTT*6\nAMT*1*13045.94\nSE*33*000000010\nGE*1*1421\nIEA*1*000003438\n"

testFail :: Text
testFail = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><note><to> Tove</to><from>Jani</from><body>Don't forget me this weekend!</body></note>"

testISA :: Text
testISA = "ISA*01*0000000000*01*ABCCO     *12*4405197800     *01*999999999      *101127*1719*U*00400*000003438*0*P*>\n"

testSegment :: Text
testSegment = "GS*PO*4405197800*999999999*20101127*1719*1421*X*004010VICS\n"

terminChar :: Char
terminChar = '\n'

isaParser :: Parser Interchange
isaParser = do
  sid <- string "ISA"
  sep <- anyChar
  elems <- count 15 (element1 sep <* char sep)
  delim <- anyChar
  termin <- anyChar
  return $ Interchange (Segment sid (sid:elems)) []

element1 :: Char -> Parser Element
element1 sepChar = takeWhile $ (\c -> c /= sepChar)

elementList1 :: Char -> Parser [Element]
elementList1 sep = sepBy (element1 sep) $ char sep

elementP :: Char -> Char -> Parser Element
elementP sepChar termChar = takeWhile (`notElem` [sepChar, termChar])

elementList :: Char -> Char-> Parser [Element]
elementList sepChar termChar = sepBy (element sepChar termChar) $ char sepChar

segmentParser :: Char -> Parser Segment
segmentParser sepChar = do
  sid <- element sepChar terminChar
  char sepChar
  elems <- elementList sepChar terminChar
  return $ Segment sid (sid:elems)

interchangeParser :: Parser [Segment]
interchangeParser = do
  sid <- string "ISA"
  sepChar <- anyChar
  elems <- elementList sepChar terminChar
  endOfLine
  segs <- many $ (segmentParser sepChar) <* endOfLine
  return $ [(Segment sid (sid:elems))] ++ segs

parseSegmentTok :: Either String [Text] -> [Either String Value]
parseSegmentTok (Right r) = do
  parsedElements <- zipWith parseOnly (map value isaTypes) r
  return parsedElements
parseSegmentTok (Left err) = error $ "A parsing error was found: " ++ err

fromEithers :: [Either String Value] -> Either [String] [Value]
fromEithers eithers = case lefts eithers of
  [] -> Right (rights eithers)
  _ -> Left (lefts eithers)
