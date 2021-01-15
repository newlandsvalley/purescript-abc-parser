-- | Basic support for polyphonic ABC tunes.
-- | Where inline Voice headers are present in the tune body, partition it
-- | into seperate bodies, one for each voice.
-- |
-- | In ABC, voices can be introduced like this:
-- |
-- | V:1
-- | abc ... 
-- | def ...
-- | V:2 
-- | CDE ... 
-- | EFG ....
-- |
-- | or like this:
-- |
-- | [V:1] abc ... 
-- | [V:1] def ...
-- | [V:2] CDE ...
-- | [V:2] EFG
-- |
-- | which means that we either have a self-standing voice header before each group 
-- | of lines (which inherit this voice) or else we have an inline voice header defined 
-- | explictly against each line.
-- |
-- | Of course, there is nothing in the spec which prevents degenerate cases where 
-- | both forms are present.  In this case, we'll assume inline headers take precedence
module Data.Abc.Voice (partitionTuneBody) where

import Prelude (class Eq, class Ord, ($), (<<<), join, map, not)
import Data.Abc (Bar, BodyPart(..), Header(..), Music(..), TuneBody)
import Data.Abc.Metadata (isEmptyStave)
import Data.List (List, head, singleton, snoc)
import Data.Maybe (Maybe(..))
import Data.Map (Map, empty, lookup, insert, toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)

data VoiceLabel = 
    VoiceLabel String
  | NoLabel

derive instance eqVoiceLabel :: Eq VoiceLabel  
derive instance ordVoiceLabel :: Ord VoiceLabel

type VoiceMap = Map VoiceLabel TuneBody

partitionTuneBody :: TuneBody -> Array TuneBody
partitionTuneBody b =
  let
    foldf :: VoiceMap -> BodyPart -> VoiceMap
    foldf vmap bp =
      case bp of
        BodyInfo header -> 
          case header of 
            Voice voiceDescription ->
               addAtLabel (VoiceLabel voiceDescription.id) bp vmap
            _ -> 
               addToAll bp vmap
        Score bars ->
          if (not $ isEmptyStave bars) then
            let 
              inlineLabel = scoreLabel bars
            in
              addAtLabel inlineLabel bp vmap
          else
            vmap
    voiceMap = foldl foldf (empty :: VoiceMap) b
  in
    map (\(Tuple k v) -> v) $ toUnfoldable voiceMap

-- append a body part at the specified map label
addAtLabel :: VoiceLabel -> BodyPart -> VoiceMap -> VoiceMap
addAtLabel label bp map =
  case (lookup label map) of
    Just body -> insert label (snoc body bp) map
    _ -> insert label (singleton bp) map

-- append a body part to all labels in the map
addToAll :: BodyPart -> VoiceMap -> VoiceMap
addToAll bp vmap =
  map (\v -> (snoc v bp)) vmap

scoreLabel :: List Bar -> VoiceLabel
scoreLabel bars =
  let
    firstBarMusic = join $ map (head <<< _.music) $ head bars
  in
    case firstBarMusic of
      (Just (Inline header)) -> voiceLabel header
      _ -> NoLabel

voiceLabel :: Header -> VoiceLabel
voiceLabel h =
  case h of
    (Voice description) -> VoiceLabel description.id
    _ -> NoLabel

