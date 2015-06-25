{-# LANGUAGE OverloadedStrings #-}
-- | Parser for ANSI X12 EDI Data Format

module X12.Parser where
import Prelude hiding (concat, takeWhile, take)
import Data.Text (Text)
import Data.Time.Calendar (Day(..), fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Format
import Data.Attoparsec.Text
import Data.Scientific (Scientific)
import Control.Applicative (many, (<*),(*>),(<*>),(<|>),(<$>))

type Element = Text
type Identifier = Text
type FunctionalGroup = [TransactionSet]

data Value = TextVal Text
             | DayVal Day
             | TimeVal TimeOfDay
             | NumVal Scientific
             deriving (Eq, Show)

valueParser :: Char -> Char -> Parser Value
valueParser sepChar termChar = DayVal  <$> dayParser
                           <|> TimeVal <$> timeParser
                           <|> NumVal  <$> scientific
                           <|> TextVal <$> textParser sepChar termChar
                           <* eitherP (char sepChar) (char termChar)

dayParser :: Parser Day
dayParser = do
  yyyy <- count 4 digit
  mm <- count 2 digit
  dd <- count 2 digit
  return $ fromGregorian (read yyyy) (read mm)  (read dd)

timeParser :: Parser TimeOfDay
timeParser = do
  hh <- count 2 digit
  mm <- count 2 digit
  ss <- count 2 digit
  return $ TimeOfDay (read hh) (read mm) (read ss)

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

data Loop =
  Loop { loopId :: Text
       , segments :: [Segment]
       }
  deriving Show

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

isaParser :: Parser Segment
isaParser = do
  sid <- string "ISA"
  sep <- anyChar
  elems <- elementList sep terminChar
  return $ Segment sid (sid:elems)

element :: Char -> Char -> Parser Element
element sepChar termChar = takeWhile (`notElem` [sepChar, termChar])

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
