{-# LANGUAGE OverloadedStrings #-}

module X12.Definitions.SegmentDefs where
import X12.Definitions.ElementUse
import Data.Text

data SegmentDef = SegmentDef { segmentId :: Text
                             , elementUses :: [ElementUse]
                             }
                deriving Show
