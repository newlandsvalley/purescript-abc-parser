-- | Variant Repeats
-- |
-- | support for the ABC volta construction:
-- |    ..|1 ... :|2 ...:|3 ....  etc 
-- |
-- | Up to 8 such variant endings are allowed in any section
module Data.Abc.Repeats.Variant
  ( activeVariants
  , initialVariantEndings
  , secondEnding
  , setVariantOf
  , setVariantList
  , variantEndingOf
  , variantIndexMax
  , variantCount) where

import Prelude (($), (-), map, join)
import Data.Abc.Repeats.Types (Section(..))
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), isJust, fromJust)
import Partial.Unsafe (unsafePartial)

-- | the active variants - i.e. those non-Nothing entries at the start of the array
activeVariants :: Section -> Array Int
activeVariants (Section s) =
  map (unsafePartial fromJust) $
    Array.takeWhile isJust s.variantEndings

-- | initialise the variant endings to none - 8 allowed
initialVariantEndings :: Array (Maybe Int)
initialVariantEndings = 
  [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

-- | the variant ending of the specified variant number 
variantEndingOf :: Int -> Section -> Maybe Int
variantEndingOf n (Section s) =   
  join $ Array.index s.variantEndings n  

-- | the second variant ending
secondEnding :: Section -> Maybe Int
secondEnding s = 
  variantEndingOf 1 s

-- set a repeat variant of a section
-- variantNo is the number of the variant
-- barNo is the bar in which this variant marking is found
setVariantOf :: Int -> Int -> Section -> Section
setVariantOf variantNo barNo (Section s) =
  let  
    variantEndings = updateVariantBarPos s.variantEndings barNo variantNo
  in
  Section s { variantEndings = variantEndings, repeatCount = 1  }

 -- set the list of variants having this bar number position
setVariantList :: Array Int -> Int -> Section -> Section
setVariantList variants barNo (Section s) =   
  let 
    -- the update defintions sets each variant to be associated with the barNo
    updateDefinition = map (\variantNo -> Tuple variantNo (Just barNo)) variants
    variantEndings :: Array (Maybe Int)
    variantEndings = updateAllVariantIndices updateDefinition s.variantEndings
  in    
    Section s { variantEndings = variantEndings, repeatCount = 1 }
    
-- | i.e. ABC variant numberings must be consecutive)
variantCount :: Section -> Int
variantCount (Section s) =
  Array.length $ Array.takeWhile isJust s.variantEndings

-- | the maximum index we can use of the active variants
variantIndexMax :: Section -> Int
variantIndexMax section = 
  variantCount section - 1

-- update the variant ending for the nominated variant and its bar number position
-- if it fails for any reason, return the original array of variant positions
updateVariantBarPos :: Array (Maybe Int) -> Int -> Int -> Array (Maybe Int)
updateVariantBarPos variantEndings barNo variantNo  = 
  case Array.updateAt variantNo (Just barNo) variantEndings of 
    Nothing -> variantEndings 
    Just updatedEndings -> updatedEndings

-- update a bunch of variant endings with the same bar number position
updateAllVariantIndices :: Array (Tuple Int (Maybe Int)) -> Array (Maybe Int) -> Array (Maybe Int)
updateAllVariantIndices barNoUpdates variantEndings =     
  Array.updateAtIndices barNoUpdates variantEndings 


