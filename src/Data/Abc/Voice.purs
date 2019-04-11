-- | Basic support for polyphonic ABC tunes.
-- | Where inline Voice headers are present in the tune body, partition it
-- | into seperate bodies, one for each voice.
-- | Note that voice headers in the initial TuneHeaders section are ignored here.
module Data.Abc.Voice (partitionTuneBody) where

import Prelude (($), (<<<), join, map, not)
import Data.Abc (Bar, BodyPart(..), Header(..), Music(..), TuneBody)
import Data.Abc.Metadata (isEmptyStave)
import Data.List (List, head, singleton, snoc)
import Data.Maybe (Maybe(..))
import Data.Map (Map, empty, lookup, insert, toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)

type VoiceMap = Map String TuneBody

-- artificial voice label for a Score item with no explicit label
noLabel :: String
noLabel =
  "noLabel"

-- | partition the tune body into separate bodies for each voice
-- | (where present).  If not present, the result is just a singleton voice.
partitionTuneBody :: TuneBody -> Array TuneBody
partitionTuneBody b =
  let
    foldf :: VoiceMap -> BodyPart -> VoiceMap
    foldf vmap bp =
      case (bodyPartLabel bp) of
        "bodyInfoLabel" -> addToAll bp vmap
        otherLabel ->
          if (not $ isEmptyBodyPart bp) then
            addAtLabel otherLabel bp vmap
          else
            vmap
    voiceMap = foldl foldf (empty :: VoiceMap) b
  in
    map (\(Tuple k v) -> v) $ toUnfoldable voiceMap

-- append a body part at the specified map label
addAtLabel :: String -> BodyPart -> VoiceMap -> VoiceMap
addAtLabel label bp map =
  case (lookup label map) of
    Just body -> insert label (snoc body bp) map
    _ -> insert label (singleton bp) map

-- append a body part to all labels in the map
addToAll :: BodyPart -> VoiceMap -> VoiceMap
addToAll bp vmap =
  map (\v -> (snoc v bp)) vmap

bodyPartLabel :: BodyPart -> String
bodyPartLabel bp =
  case bp of
    (BodyInfo _) -> "bodyInfoLabel"
    (Score bars) -> scoreLabel bars

scoreLabel :: List Bar -> String
scoreLabel bars =
  let
    firstBarMusic = join $ map (head <<< _.music) $ head bars
  in
    case firstBarMusic of
      (Just (Inline header)) -> voiceLabel header
      _ -> noLabel

voiceLabel :: Header -> String
voiceLabel h =
  case h of
    (Voice description) -> description.id
    _ -> noLabel

isEmptyBodyPart :: BodyPart -> Boolean
isEmptyBodyPart bp =
  case bp of
    (BodyInfo _) -> false
    (Score bars) -> isEmptyStave bars
