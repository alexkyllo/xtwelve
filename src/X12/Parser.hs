{-# LANGUAGE OverloadedStrings #-}
-- | Parser for ANSI X12 EDI Data Format

module X12.Parser where
import Prelude hiding (concat, takeWhile)
import Data.Text (Text, pack)
import Data.Attoparsec.Text
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)

type Element = Text

testFile :: Text
testFile = "ISA*01*0000000000*01*ABCCO     *12*4405197800     *01*999999999      *101127*1719*U*00400*000003438*0*P*>"

testFail :: Text
testFail = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><note><to> Tove</to><from>Jani</from><body>Don't forget me this weekend!</body></note>"

sepChar :: Char
sepChar = '*'

segmentIdParser :: Parser Element
segmentIdParser = do
  sid <- count 3 (letter <|> digit)
  return $ pack sid


element :: Parser Element
element = takeWhile (/= sepChar)

elementList :: Parser [Element]
elementList = element `sepBy` char sepChar

segmentParser :: Parser [Element]
segmentParser = do
  sid <- segmentIdParser
  elems <- elementList
  return $ sid : elems
