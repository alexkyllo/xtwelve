{-# LANGUAGE OverloadedStrings #-}
-- | Tokenizer for ANSI X12 EDI Data Format

module X12.Tokenizer where

import Prelude hiding (concat, takeWhile, take)
import Data.Text (Text)
import Data.Attoparsec.Text
import Control.Applicative (pure, many, (<*), (*>),(<*>),(<|>),(<$>))
import X12.Separators
import X12.Tokens.SegmentToken
import X12.Tokens.ElementToken

type ElementTok = Text
type SegmentTok = [ElementTok]
--type InterchangeTokens = [Segment]

-- | parse an ElementToken when you only know the element separator
element1 :: Char -> Parser ElementTok
element1 sep = takeWhile (/= sep)

-- | parse an ElementToken using a full set of known separators
element :: Separators -> Parser ElementTok
element seps = takeWhile (`notElem` [elementSeparator seps, segmentSeparator seps])

segment :: Separators -> Parser SegmentTok
segment seps = sepBy (element seps) $ char (elementSeparator seps)

-- | read an interchange, parsing the ISA segment first to determine what separators are used
parseInterchange :: Parser ([SegmentTok], Separators)
parseInterchange = do
  i <- string "ISA" -- every Interchange starts with the text "ISA"
  elementSep <- anyChar -- followed by any single character, which will be the element separator
  isaElements <- count 15 $ element1 elementSep <* char elementSep -- parse the next 15 element tokens
  componentSep <- anyChar -- ISA16 is the component separator
  segmentSep <- anyChar -- the next character is the segment separator
  seps <- pure $ Separators { componentSeparator = componentSep
                            , repetitionSeparator = '\\'
                            , elementSeparator = elementSep
                            , segmentSeparator = segmentSep
                            }
  segments <- many $ (segment seps) <* (char (segmentSeparator seps))
  return $ ([i:isaElements] ++ segments, seps)

tokenizeISA :: Parser (SegmentToken, Separators)
tokenizeISA = do
  i <- string "ISA" -- every Interchange starts with the text "ISA"
  elementSep <- anyChar -- followed by any single character, which will be the element separator
  isaElements <- count 15 $ element1 elementSep <* char elementSep -- parse the next 15 element tokens
  componentSep <- anyChar -- ISA16 is the component separator
  segmentSep <- anyChar -- the next character is the segment separator
  seps <- pure $ Separators { componentSeparator = componentSep
                            , repetitionSeparator = '^'
                            , elementSeparator = elementSep
                            , segmentSeparator = segmentSep
                            }
  elementTokens <- pure $ map SimpleElementToken isaElements
  return $ (SegmentToken "ISA" elementTokens, seps)

tokenizeSegment :: Separators -> Parser SegmentToken
tokenizeSegment seps = do
  segmentID <- take 3
  char (elementSeparator seps)
  return $ SegmentToken "ISA" []
