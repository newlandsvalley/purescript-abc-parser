-- | Basic support for polyphonic ABC tunes.
-- |
-- | Where voice headers are present in the tune body, partition it
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
-- |
-- | The strategy is to fold over the tune structure in the State monad.  State will be 
-- | changed each time we come across a free-standing Voice header in the tune body.
-- | The current voice is this unless over-ridden by an inline voice.  
-- | The fold builds up a Map of Voices to partitioned ABC tune bodies
module Data.Abc.Voice 
  ( partitionVoices
  , partitionTuneBody) where

import Prelude (class Eq, class Ord, ($), (<<<), bind, join, map, not, pure)
import Data.Abc (AbcTune, Bar, BodyPart(..), Header(..), Music(..), TuneBody)
import Data.Abc.Metadata (getHeaders, isEmptyStave)
import Data.List (List, head, last, singleton, snoc)
import Data.Maybe (Maybe(..))
import Data.Map (Map, empty, lookup, insert, toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldM)
import Data.Identity (Identity(..))
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.State.Class (get, put)

data VoiceLabel = 
    VoiceLabel String
  | NoLabel

derive instance eqVoiceLabel :: Eq VoiceLabel  
derive instance ordVoiceLabel :: Ord VoiceLabel

type VoiceMap = Map VoiceLabel TuneBody

type VoiceM = StateT VoiceLabel Identity

-- | given a tune, partition it into multiple such tunes
-- | one for each voice
partitionVoices :: AbcTune -> Array AbcTune 
partitionVoices tune = 
  map (\body -> {headers : tune.headers, body}) (partitionTuneBody tune)

-- | given a tune, partition its body into multiple such bodies 
-- | with a separate body for each distinct voice
partitionTuneBody :: AbcTune -> Array TuneBody
partitionTuneBody tune =    
  let
    initialVoiceLabel = case (last $ getHeaders 'V' tune) of
      Just (Voice description) -> VoiceLabel description.id 
      _ -> NoLabel
    voiceMap = runVoiceM initialVoiceLabel (voiceFold tune.body) 
  in 
    map (\(Tuple k v) -> v) $ toUnfoldable voiceMap

runVoiceM :: forall a. VoiceLabel -> VoiceM a -> a
runVoiceM initialVoiceLabel v =
  let
    (Identity a) = evalStateT v initialVoiceLabel
  in 
    a

-- The heart of the algorithm
-- Track voice labels for all score lines in the tune and separate 
-- parts of the tune with distinct voices into distinct tunes
-- meanwhile ensuring any common constructs are shared by all tune partitions
voiceFold :: TuneBody -> VoiceM VoiceMap
voiceFold b = 
  let  
    foldf :: VoiceMap -> BodyPart -> VoiceM VoiceMap
    foldf vmap bp = do
      case bp of
        BodyInfo header -> 
          case header of 
            Voice voiceDescription -> do
              _ <- put (VoiceLabel voiceDescription.id)
              pure $ addAtLabel (VoiceLabel voiceDescription.id) bp vmap
            _ -> 
              pure $ addToAll bp vmap
        Score bars -> do
          currentVoice <- get
          if (not $ isEmptyStave bars) then
            pure $ addAtLabel (scoreLabel currentVoice bars) bp vmap
          else
            pure vmap
  in
    foldM foldf (empty :: VoiceMap) b

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

-- find the inline voice label from a score line (if it exists)
-- otherwise fall back to the default voice
scoreLabel :: VoiceLabel -> List Bar -> VoiceLabel
scoreLabel currentVoice bars  =
  let
    firstBarMusic = join $ map (head <<< _.music) $ head bars
  in
    case firstBarMusic of
      (Just (Inline header)) -> voiceLabel currentVoice header 
      _ -> currentVoice

-- find the voice label from an inline header (if it defines a voice)
-- otherwise fall back to the default voice
voiceLabel :: VoiceLabel -> Header -> VoiceLabel
voiceLabel currentVoice h =
  case h of
    (Voice description) -> VoiceLabel description.id
    _ -> currentVoice

