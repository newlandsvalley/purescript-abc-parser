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
  , partitionTuneBody
  ) where

import Control.Monad.State.Class (get, put, modify)
import Control.Monad.State.Trans (StateT, evalStateT)
import Data.Abc (AbcTune, Bar, BodyPart(..), Header(..), Music(..), TuneBody, TuneHeaders)
import Data.Abc.Utils (isEmptyStave)
import Data.Abc.Optics (_headers, _Voice, _Title)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..)) as Unsafe
import Data.Foldable (foldM)
import Data.Identity (Identity)
import Data.Lens.Fold (firstOf, lastOf)
import Data.Lens.Traversal (traversed)
import Data.Lens.Setter (over)
import Data.List (List, (:), filter, head, singleton, snoc)
import Data.Map (Map, empty, fromFoldable, lookup, insert, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set, empty, insert, toUnfoldable) as Set
import Data.Tuple (Tuple(..), snd)
import Prelude (class Eq, class Ord, ($), (==), (<<<), (<>), bind, join, map, not, pure)

data VoiceLabel
  = VoiceLabel String
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
-- | if there is no voice header, then the voice name is "unnamed" and attached to 
-- | the entire tune.
-- | If voices are found, then the title of each partitioned voice tune is set to
-- | 'Voice voice-name'
getVoiceMap :: AbcTune -> Map String AbcTune
getVoiceMap tune =
  fromFoldable $ map (retitleFromVoiceLabel tune) tuples

  where
  tuples :: Array (Tuple VoiceLabel TuneBody)
  tuples = toUnfoldable $ voiceMap tune

-- | given a tune, partition it into multiple such tunes, one for each voice
-- | with the title of each partitioned tune set to the voice label
-- | where there are no voices, just return the singleton which contains
-- | the original tune
partitionVoices :: AbcTune -> NonEmptyArray AbcTune
partitionVoices tune =
  map (snd <<< retitleFromVoiceLabel tune) tuples

  where
  tuples :: NonEmptyArray (Tuple VoiceLabel TuneBody)
  tuples = Unsafe.NonEmptyArray $ toUnfoldable $ voiceMap tune

-- | given a tune, partition its body into multiple such bodies 
-- | with a separate body for each distinct voice
-- | where there are no voices, just return the singleton which contains
-- | the original tune body
partitionTuneBody :: AbcTune -> NonEmptyArray TuneBody
partitionTuneBody tune =
  map (\(Tuple _ v) -> v) $ Unsafe.NonEmptyArray $ toUnfoldable (voiceMap tune)

-- produce a map of voice label to tune (filtered for that voice only)
-- note that there must be at least one entry in the map because the common
-- case is when there are no voices and there is one entry labelled 'unnnamed'
-- and attached to the entire tune
voiceMap :: AbcTune -> VoiceMap
voiceMap tune =
  runVoiceM (initialVoiceLabel tune) (voiceFold tune.body)

runVoiceM :: forall a. VoiceLabel -> VoiceM a -> a
runVoiceM initialLabel v =
  unwrap $ evalStateT v initialLabel

runLabelM :: forall a. Set.Set String -> LabelM a -> a
runLabelM initialLabels v =
  unwrap $ evalStateT v initialLabels

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
scoreLabelOrDefault currentVoiceLabel bars =
  case (inlineLabel bars) of
    Just label -> label
    _ -> currentVoiceLabel

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
  case (lastOf (_headers <<< traversed <<< _Voice) tune) of
    Just description -> VoiceLabel description.id
    _ -> NoLabel

-- map the keys to String and the values to AbcTune
retitleFromVoiceLabel :: AbcTune -> Tuple VoiceLabel TuneBody -> Tuple String AbcTune
retitleFromVoiceLabel tune (Tuple k body) =
  case k of
    VoiceLabel name ->
      Tuple name { headers: newHeaders, body }
      where
      newHeaders = retitle name tune.headers
    NoLabel ->
      Tuple "unnamed" { headers: tune.headers, body }

  where

  -- retitle the headers by replacing any original tune title 
  -- with the voice name 
  retitle :: String -> TuneHeaders -> TuneHeaders
  retitle voiceName headers =
    case (firstOf (traversed <<< _Title) headers) of
      Just _ ->
        over (traversed <<< _Title) (\t -> t <>" - voice " <> voiceName) filteredHeaders
        -- set (traversed <<< _Title) ("voice " <> voiceName) filteredHeaders

        where 
        predicate :: Header -> Boolean
        predicate h =
          case h of
            -- remove any voice header for a different voice from the one we're handling
            -- i.e. only keep the voice header we're handling
            Voice voiceDescription -> voiceDescription.id == voiceName
            _ -> true
        filteredHeaders = filter predicate headers

      _ ->
        ReferenceNumber (Just 1) : Title ("voice " <> voiceName) : filteredRetitledHeaders

        where
        predicate :: Header -> Boolean
        predicate h =
          case h of
            -- remove the reference number
            ReferenceNumber _ -> false
            -- remove the old title
            Title _ -> false
            -- remove any voice header for a different voice from the one we're handling
            -- i.e. only keep the voice header we're handling
            Voice voiceDescription -> voiceDescription.id == voiceName
            _ -> true
        filteredRetitledHeaders = filter predicate headers

        

