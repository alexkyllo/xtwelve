{-# LANGUAGE OverloadedStrings #-}
-- | Functions and types for parsing ANSI X12 data types

module X12.Parser.Value where
import Prelude hiding (concat, takeWhile, take)
import Data.Attoparsec.Text
import Data.Text (Text(..), pack)
import Data.Time.Calendar (Day(..), fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Format
import Data.Scientific (Scientific)
import Control.Applicative (pure, many, (<*),(*>),(<*>),(<|>),(<$>))

data Value = AN Text
             | ID Text
             | DT Day
             | TM TimeOfDay
             | R  Scientific
             | N  Integer
             deriving (Eq, Show)

value :: Text -> Parser Value
value valtype = case valtype of
  "DT" -> DT <$> (dayParser8 <|> dayParser6)
  "TM" -> TM <$> (timeParser6 <|> timeParser4)
  "R"  -> R  <$> scientific
  "N"  -> N  <$> (signed decimal)
  "ID" -> ID <$> (takeText)
  otherwise -> AN <$> (takeText)

parseDT :: Parser Value
parseDT = DT <$> (dayParser8 <|> dayParser6)

parseTM :: Parser Value
parseTM = TM <$> (timeParser6 <|> timeParser4)

parseR :: Parser Value
parseR = R <$> scientific

parseN :: Parser Value
parseN = N <$> (signed decimal)

parseID :: Parser Value
parseID = ID <$> takeText

parseAN :: Parser Value
parseAN = AN <$> takeText

dayParser8 :: Parser Day
dayParser8 = do
  yyyy <- count 4 digit
  mm <- count 2 digit
  dd <- count 2 digit
  return $ fromGregorian (read yyyy) (read mm)  (read dd)

dayParser6 :: Parser Day
dayParser6 = do
  yy <- count 2 digit
  mm <- count 2 digit
  dd <- count 2 digit
  return $ fromGregorian (read ("20" ++ yy)) (read mm) (read dd)

timeParser6 :: Parser TimeOfDay
timeParser6 = do
  hh <- count 2 digit
  mm <- count 2 digit
  ss <- count 2 digit
  return $ TimeOfDay (read hh) (read mm) (read ss)

timeParser4 :: Parser TimeOfDay
timeParser4 = do
  hh <- count 2 digit
  mm <- count 2 digit
  return $ TimeOfDay (read hh) (read mm) 00

textParser :: Char -> Char -> Parser Text
textParser sep term = takeWhile (`notElem` [sep, term])
