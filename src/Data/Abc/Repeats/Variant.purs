-- | Variant Repeats
-- |
-- | support for the ABC volta construction:
-- |    ..|1 ..... :|2 ...:|3 ....  
-- | or ..|1,2,3.. :|4.... |    etc 
-- |
module Data.Abc.Repeats.Variant
  ( activeVariants
  , secondVariantPosition
  , setVariantOf
  , setVariantList
  , variantPositionOf
  , variantIndexMax
  , variantCount) where

import Prelude (($))
import Data.Abc.Repeats.Types (Section(..), VariantPositions)
import Data.Array as Array
import Data.Map (insert, keys, lookup, size, toUnfoldable)
import Data.Set (findMax)
import Data.Tuple (Tuple)
import Data.Maybe (Maybe, fromMaybe)

-- | the active variants returned as an array of tuples (variant - position)
activeVariants :: Section -> Array (Tuple Int Int)
activeVariants (Section s) =
  toUnfoldable s.variantPositions

-- | the variant position of the specified variant number 
variantPositionOf :: Int -> Section -> Maybe Int
variantPositionOf n (Section s) =   
  lookup n s.variantPositions

-- | the position of the second variant 
secondVariantPosition :: Section -> Maybe Int
secondVariantPosition s = 
  variantPositionOf 1 s

-- set a repeat variant of a section
-- variantNo is the number of the variant
-- barNo is the bar in which this variant marking is found
setVariantOf :: Int -> Int -> Section -> Section
setVariantOf variantNo barNo (Section s) =
  let  
    variantPositions = insert variantNo barNo s.variantPositions
  in
  Section s { variantPositions = variantPositions, repeatCount = 1  }

 -- set the list of variants having this bar number position
setVariantList :: Array Int -> Int -> Section -> Section
setVariantList variants barNo (Section s) =   
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
insertAllVariantIndices :: Array Int -> Int -> VariantPositions -> VariantPositions
insertAllVariantIndices variants barNo variantPositions =     
  let 
    f :: Int -> VariantPositions -> VariantPositions
    f v positions = 
      insert v barNo positions
  in
    Array.foldr f variantPositions variants


