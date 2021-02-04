-- | Variant Repeats
-- |
-- | support for the ABC volta construction:
-- |    ..|1 ..... :|2 ...:|3 ....  
-- | or ..|1,2,3.. :|4.... |    etc 
-- |
module Data.Abc.Repeats.Variant
  ( activeVariants
  , secondVariantPosition
  , addVariantOf
  , addVariantList
  , findEndingPosition
  , variantPositionOf
  , variantIndexMax
  , variantCount) where

import Prelude (($), (>))
import Data.Abc.Repeats.Types (BarNo, Section(..), VariantPositions)
import Data.Array as Array
import Data.Map (filter, insert, keys, lookup, size, toUnfoldable)
import Data.Set (findMin, findMax)
import Data.Tuple (Tuple)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Partial.Unsafe (unsafePartial)
  

-- | the active variants returned as an array of tuples (variant no - bar no)
activeVariants :: Section -> Array (Tuple Int BarNo)
activeVariants (Section s) =
  toUnfoldable s.variantPositions

-- | the variant bar number of the specified variant number 
variantPositionOf :: Int -> Section -> Maybe BarNo
variantPositionOf n (Section s) =   
  lookup n s.variantPositions

-- | the bar number of the second variant 
secondVariantPosition :: Section -> Maybe BarNo
secondVariantPosition s = 
  variantPositionOf 1 s

-- | add a repeat variant of a section to the existing variants
-- | variantNo is the number of the variant
-- | barNo is the bar in which this variant marking is found
addVariantOf :: Int -> BarNo -> Section -> Section
addVariantOf variantNo barNo (Section s) =
  let  
    variantPositions = insert variantNo barNo s.variantPositions
  in
  Section s { variantPositions = variantPositions, repeatCount = 1  }

 -- | add the list of variants having this bar number position to the existing variants
addVariantList :: Array Int -> BarNo -> Section -> Section
addVariantList variants barNo (Section s) =   
  let 
    -- the update defintions sets each variant to be associated with the barNo
    variantPositions :: VariantPositions
    variantPositions = insertAllVariantIndices variants barNo s.variantPositions
  in    
    Section s { variantPositions = variantPositions, repeatCount = 1 }
    
-- | the number of variants in the ending
variantCount :: Section -> Int
variantCount (Section s) =
  size s.variantPositions

-- | the maximum index we can use of the active variants
variantIndexMax :: Section -> Int
variantIndexMax (Section s) = 
  let 
    variantIndices = keys s.variantPositions
  in 
    fromMaybe 0 $ findMax variantIndices

-- set a bunch of variant positions with the same bar number position
insertAllVariantIndices :: Array Int -> BarNo -> VariantPositions -> VariantPositions
insertAllVariantIndices variants barNo variantPositions =     
  let 
    f :: Int -> VariantPositions -> VariantPositions
    f v positions = 
      insert v barNo positions
  in
    Array.foldr f variantPositions variants

-- | When supplied with:
-- | the variant positions (map of index to BarNo)
-- | the index of the current variant 
-- | the BarNo of the end of the entire section
-- | then find the BarNo of the end of the variant
-- | (This can either be the start of the next variant or the section end)
findEndingPosition :: VariantPositions -> Int -> BarNo -> BarNo 
findEndingPosition variantPositions index end = 
  -- find the position of the variant at this index
  case (lookup index variantPositions) of 
    Nothing -> 
      end 
    Just thisPos ->  
      let 
        -- find all variants with bar positions greater than our position
        candidates = filter (_ > thisPos) variantPositions 
        -- find the minimum key amongst these candidates
        mNext = findMin $ keys candidates
      in 
        case mNext of 
          Nothing ->
            -- nothing there - default to end
            end 
          Just next ->
            -- look it up - it will never fail because we know the key exists
            unsafePartial $ fromJust $ lookup next candidates    


