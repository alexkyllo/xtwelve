{-# LANGUAGE OverloadedStrings #-}

module X12.Definitions.SegmentDef where
import X12.Definitions.ElementUse
import Data.Text

data SegmentDef = SegmentDef { segmentId :: Text
                             , segmentName :: Text
                             , segmentPurpose :: Text
                             , elementUses :: [ElementUse]
                             }
                deriving Show
