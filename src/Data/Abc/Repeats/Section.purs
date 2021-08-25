-- | Utility functions to deal with Repeated Sections
module Data.Abc.Repeats.Section
  ( hasFirstEnding
  , isDeadSection
  , isUnrepeated
  , newSection
  , nullSection
  , setEndPos
  , setMissingRepeatCount
  , toOffsetZero
  ) where

import Data.Abc.Repeats.Types (BarNo, Label(..), Section(..))
import Data.Abc.Repeats.Variant (variantPositionOf)
import Data.Map (empty)
import Data.Maybe (Maybe(..), isJust)
import Prelude ((==), (<=), (-))

-- | volta repeat markers are wrt offset 1 - reduce to 0
toOffsetZero :: Int -> Int
toOffsetZero i =
  if i <= 0 then 0 else i - 1

-- start a new section
newSection :: BarNo -> Int -> Section
newSection pos repeatCount =
  Section
    { start: Just pos
    , variantPositions: empty
    , end: Just 0
    , repeatCount: repeatCount
    , label: OtherPart -- not used here
    }

-- a 'null' section
nullSection :: Section
nullSection =
  newSection 0 0

-- return true if the section is devoid of any useful content
isDeadSection :: Section -> Boolean
isDeadSection s =
  s == nullSection

-- return true if the repeat count of a section is not set
isUnrepeated :: Section -> Boolean
isUnrepeated (Section s) =
  s.repeatCount == 0

-- return true if the first (variant) ending is set
hasFirstEnding :: Section -> Boolean
hasFirstEnding s =
  isJust (variantPositionOf 0 s)

-- set the repeatedCount status of a section if it is missing
-- (in the wild we get   |......|.....:| etc. with no start colon)
setMissingRepeatCount :: Section -> Section
setMissingRepeatCount (Section s) =
  Section s { repeatCount = 1 }

-- set the end BarNo position of a section
setEndPos :: BarNo -> Section -> Section
setEndPos pos (Section s) =
  Section s { end = Just pos }

