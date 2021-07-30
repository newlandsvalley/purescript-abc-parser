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
  ( getVoiceLabels
  , getVoiceMap
  , partitionVoices
  , partitionTuneBody) where

import Control.Monad.State.Class (get, put, modify)
import Control.Monad.State.Trans (StateT, evalStateT)
import Data.Abc (AbcTune, Bar, BodyPart(..), Header(..), Music(..), TuneBody, TuneHeaders)
import Data.Abc.Metadata (getHeaders, isEmptyStave)
import Data.Foldable (foldM)
import Data.Identity (Identity(..))
import Data.List (List, (:), head, filter, last, singleton, snoc)
import Data.Map (Map, empty, fromFoldable, lookup, insert, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, insert, toUnfoldable) as Set
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Ord, ($), (<<<), (<>), bind, join, map, not, pure)

data VoiceLabel = 
    VoiceLabel String
  | NoLabel

derive instance eqVoiceLabel :: Eq VoiceLabel  
derive instance ordVoiceLabel :: Ord VoiceLabel

type VoiceMap = Map VoiceLabel TuneBody

type VoiceM = StateT VoiceLabel Identity

type Labels = Set.Set String

type LabelM = StateT Labels Identity

-- | given a tune, find all the different voice names (labels)
-- | no matter where they might be hiding
getVoiceLabels :: AbcTune -> Array String
getVoiceLabels tune =
  let 
    initialLabels :: Set.Set String
    initialLabels =
      case (initialVoiceLabel tune) of 
        NoLabel -> (Set.empty :: Labels)
        VoiceLabel label -> Set.insert label (Set.empty :: Labels)
    voiceLabels = 
      runLabelM initialLabels (labelFold tune.body) 
  in
    Set.toUnfoldable voiceLabels

-- | get a map of voice name to tune (filtering the body for just that tune voice)
-- | if there is no voice header, then the voice name is "unnamed"
-- | If voices are found, then the title of the partitioned voice tune is set to
-- | 'Voice voice-name'
getVoiceMap :: AbcTune -> Map String AbcTune 
getVoiceMap tune = 
  let 
    tuples :: Array (Tuple VoiceLabel TuneBody)
    tuples = toUnfoldable $ voiceMap tune
    -- map the keys to String and the values to AbcTune
    f :: Tuple VoiceLabel TuneBody -> Tuple String AbcTune
    f (Tuple k body) = 
      case k of 
        VoiceLabel name -> 
          Tuple name {headers : newHeaders, body}
            where 
              newHeaders = retitle name tune.headers
        NoLabel -> 
          Tuple "unnamed" {headers : tune.headers, body}
  in 
    fromFoldable $ map f tuples

-- | given a tune, partition it into multiple such tunes
-- | one for each voice
partitionVoices :: AbcTune -> Array AbcTune 
partitionVoices tune = 
  map (\body -> {headers : tune.headers, body}) (partitionTuneBody tune)


-- | given a tune, partition its body into multiple such bodies 
-- | with a separate body for each distinct voice
partitionTuneBody :: AbcTune -> Array TuneBody
partitionTuneBody tune =    
  map (\(Tuple _ v) -> v) $ toUnfoldable (voiceMap tune)

-- produce a map of voice label to tune (filtered for that voice only)
voiceMap :: AbcTune -> VoiceMap 
voiceMap tune =     
  let
    initialLabel = initialVoiceLabel tune
  in
    runVoiceM initialLabel (voiceFold tune.body) 

runVoiceM :: forall a. VoiceLabel -> VoiceM a -> a
runVoiceM initialLabel v =
  let
    (Identity a) = evalStateT v initialLabel
  in 
    a

runLabelM :: forall a. Set.Set String -> LabelM a -> a
runLabelM initialLabels v =
  let
    (Identity a) = evalStateT v initialLabels
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
            pure $ addAtLabel (scoreLabelOrDefault currentVoice bars) bp vmap
          else
            pure vmap
  in
    foldM foldf (empty :: VoiceMap) b

-- as above but just retrieve the set of voice labels
labelFold :: TuneBody -> LabelM Labels
labelFold b = 
  let  
    foldf :: Labels -> BodyPart -> LabelM Labels
    foldf labels bp = do
      case bp of
        BodyInfo header -> 
          case header of 
            Voice voiceDescription -> do
              newLabels <- modify (Set.insert (voiceDescription.id))
              pure newLabels
            _ -> 
              pure labels
        Score bars -> do
          if (not $ isEmptyStave bars) then do
            case (inlineLabel bars) of
              Just (VoiceLabel label) -> do
                newLabels <- modify (Set.insert label)
                pure newLabels
              _ ->           
                pure labels
          else
            pure labels
  in
    foldM foldf (Set.empty :: Labels) b    

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
scoreLabelOrDefault :: VoiceLabel -> List Bar -> VoiceLabel
scoreLabelOrDefault currentVoiceLabel bars  =
  case (inlineLabel bars) of 
    Just label -> label 
    _ -> currentVoiceLabel


{-
-- find the voice label from an inline header (if it defines a voice)
-- otherwise fall back to the default voice
voiceLabel :: VoiceLabel -> Header -> VoiceLabel
voiceLabel currentVoice h =
  case h of
    (Voice description) -> VoiceLabel description.id
    _ -> currentVoice
-}

-- if the line of music starts with an inLine voice header, return the voice
-- label, otherwise Nothing
inlineLabel :: List Bar -> Maybe VoiceLabel
inlineLabel bars = 
  let
    mFirstBarMusic = join $ map (head <<< _.music) $ head bars
  in 
    case mFirstBarMusic of
      (Just (Inline header)) ->
        case header of 
          (Voice description) -> Just (VoiceLabel description.id)
          _ -> Nothing
      _ -> Nothing

-- get the voice label from the initial headers if it exists
initialVoiceLabel :: AbcTune -> VoiceLabel
initialVoiceLabel tune = 
  case (last $ getHeaders 'V' tune) of
     Just (Voice description) -> VoiceLabel description.id 
     _ -> NoLabel    

-- retitle the headers by replacing any original tune title 
-- with the voice name (and normalising the Ref No to 1)
retitle :: String -> TuneHeaders -> TuneHeaders 
retitle voiceName headers = 
  ReferenceNumber (Just 1) : Title ("Voice " <> voiceName) : filteredHeaders

  where 
    predicate :: Header -> Boolean 
    predicate h =
      case h of 
        ReferenceNumber _ -> false 
        Title _ -> false 
        _ -> true
    filteredHeaders = filter predicate headers