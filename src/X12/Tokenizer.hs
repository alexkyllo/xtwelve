-- | Tokenizer for ANSI X12 EDI Data Format

module X12.Tokenizer where

import Prelude hiding (concat, takeWhile, take, lookup)
import Data.Text (Text)
import Data.Attoparsec.Text
import Data.Map hiding (map)
import Control.Applicative (pure, many, (<*), (*>),(<*>),(<|>),(<$>))
import X12.Tokens.SegmentToken
import X12.Tokens.ElementToken
import X12.Definitions
import X12.Definitions.SegmentDefs

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

-- | Read an interchange, parsing the ISA segment first to determine what separators are used
parseInterchange :: Parser ([SegmentTok], Separators)
parseInterchange = do
  i <- string "ISA" -- every Interchange starts with the text "ISA"
  elementSep <- anyChar -- followed by any single character, which will be the element separator
  isaElements <- count 15 $ element1 elementSep <* char elementSep -- parse the next 15 element tokens
  componentSep <- anyChar -- ISA16 is the component separator
  segmentSep <- anyChar -- the next character is the segment separator
  seps <- pure Separators { componentSeparator = componentSep
                          , repetitionSeparator = '\\'
                          , elementSeparator = elementSep
                          , segmentSeparator = segmentSep
                          }
  segments <- many $ segment seps <* char (segmentSeparator seps)
  return ((i : isaElements) : segments, seps)

-- | Tokenize the ISA (Interchange header segment) and detect the separators/delimiters being used
tokenizeISA :: Parser (SegmentToken, Separators)
tokenizeISA = do
  i <- string "ISA" -- every Interchange starts with the text "ISA"
  elementSep <- anyChar -- followed by any single character, which will be the element separator
  isaElements <- count 15 $ element1 elementSep <* char elementSep -- parse the next 15 element tokens
  componentSep <- anyChar -- ISA16 is the component separator
  segmentSep <- anyChar -- the next character is the segment separator
  seps <- pure Separators { componentSeparator = componentSep
                            , repetitionSeparator = '^'
                            , elementSeparator = elementSep
                            , segmentSeparator = segmentSep
                            }
  elementTokens <- pure $ map SimpleElementToken isaElements
  return (SegmentToken "ISA" elementTokens, seps)

-- | Tokenize any other segment after the separators are identified
tokenizeSegment :: Separators -> Parser SegmentToken
tokenizeSegment seps = do
  segmentID <- element seps
  char (elementSeparator seps)
  elementToks <- segment seps
  mSegmentDef <- pure $ lookup segmentID segmentDefs
  elemTokenC <- case mSegmentDef of
    Just (SegmentDef _ _ _ eUses) -> pure $ map elemUseToTok eUses
    Nothing -> pure []
  return $ SegmentToken segmentID (zipWith ($) elemTokenC elementToks)

elemUseToTok :: ElementUse -> Text -> ElementToken
elemUseToTok _ = SimpleElementToken
