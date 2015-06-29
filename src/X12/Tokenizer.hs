{-# LANGUAGE OverloadedStrings #-}
-- | Tokenizer for ANSI X12 EDI Data Format

module X12.Tokenizer where

import Prelude hiding (concat, takeWhile, take)
import Data.Text (Text)
import Data.Attoparsec.Text
import Control.Applicative (pure, many, (<*), (*>),(<*>),(<|>),(<$>))

type ElementToken = Text
--type SegmentTokens = [Element]
--type InterchangeTokens = [Segment]

element :: Char -> Char -> Parser ElementToken
element sep term = takeWhile (`notElem` [sep, term])

segment :: Char -> Char -> Parser [ElementToken]
segment sep term = sepBy (element sep term) $ char sep

isa :: Parser [[ElementToken]]
isa = do
  i <- string "ISA"
  sep <- anyChar
  isaElements <- segment sep '\n'
  term <- anyChar
  segments <- many $ (segment sep term) <* (char term)
  return $ [i:isaElements] ++ segments
