-- | Handle any repeated sections when interpreting an ABC tune
-- | Repeats are optional and can take the form:
-- |    |: ABC :|
-- |    |: ABC :: DEF :|
-- |    |: ABC |1 de :|2 fg |
-- | the very first repeat start marker is optional and often absent
module Data.Abc.Midi.RepeatSections
        ( initialRepeatState
        , indexBar
        , finalBar
        ) where

import Data.Abc (Volta(..))
import Data.Abc.Midi.Types (MidiBar)
import Data.Abc.Repeats.Types (Label(..), RepeatState, Section(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.NonEmpty as NonEmpty
import Prelude ((==), (>), (<=), (&&), map, not)

-- | initial repeats i.e. no repeats yet.  intro is not used in MIDI production
initialRepeatState :: RepeatState
initialRepeatState =
  { current : nullSection, sections : Nil, intro : [] }

-- | index a bar by identifying any repeat markings and saving the marking against 
-- | the bar number
-- | WARNING  at the moment we don't properly handle complete volta lists here
-- | nor do we handle multiple 'normal' repeats (n > 1)
indexBar :: MidiBar -> RepeatState -> RepeatState
indexBar mb r =
  case mb.iteration, mb.endRepeats, mb.startRepeats of
    -- |1
    Just (Volta 1), _ , _ ->
      r { current = firstRepeat mb.number r.current}
    -- |2  or :|2
    Just (Volta 2), _ , _ ->
      r { current = secondRepeat mb.number r.current}
    -- | 1,2 etc - this line does not do the job
    Just (VoltaList vs), _ , _ ->
      r { current = firstRepeat mb.number r.current}
    Nothing,  ends,  starts ->    
      if (ends > 0 && starts > 0) then
        endAndStartSection mb.number true true r
      else if (ends > 0 && starts <= 0) then
        endSection mb.number true r
      else if (ends <= 0 && starts > 0) then
        startSection mb.number r
      else 
        r
    _, _, _ -> 
      r

{-| accumulate any residual current state from the final bar in the tune -}
finalBar :: MidiBar -> RepeatState -> RepeatState
finalBar mb r =
  let
    isRepeatEnd = mb.endRepeats > 0 
    repeatState = endSection mb.number isRepeatEnd r
  in
    if not (isNullSection r.current) then
      accumulateSection mb.number false repeatState
    else
      repeatState

-- default sections i.e. no repeats yet
defaultSections :: RepeatState
defaultSections =
  { current : nullSection, sections : Nil, intro : [] }

-- accumulate the last section and start a new section  -}
startSection :: Int -> RepeatState -> RepeatState
startSection pos r =
  -- a start implies an end of the last section
  endAndStartSection pos false true r

-- end the section.  If there is a first repeat, keep it open, else accumulate it
-- pos : the bar number marking the end of section
-- isRepeatEnd : True if invoked with a known Repeat End marker in the bar line
endSection :: Int -> Boolean -> RepeatState -> RepeatState
endSection pos isRepeatEnd r =
  if (hasFirstEnding r.current) then
    let
      current = setEndPos pos r.current
    in
      r { current = current }
  else
     endAndStartSection pos isRepeatEnd false r

-- end the current section, accumulate it and start a new section
endAndStartSection :: Int -> Boolean -> Boolean -> RepeatState -> RepeatState
endAndStartSection endPos isRepeatEnd isRepeatStart r =
  let
    -- cater for the situation where the ABC marks the first section of the tune as repeated solely by use
    -- of the End Repeat marker with no such explicit marker at the start of the section - it is implied as the tune start
    current :: Section
    current =
      if (isRepeatEnd && (unwrap r.current).start == Just 0) then
        setRepeated r.current
      else
        r.current
    -- now set the end position from the bar number position
    current' = setEndPos endPos current
    -- set the new current into the state
    endState :: RepeatState
    endState = r { current = current' }
  in
    accumulateSection endPos isRepeatStart endState

-- accumulate the current section into the full score and re-initialise it
accumulateSection :: Int -> Boolean -> RepeatState -> RepeatState
accumulateSection pos isRepeatStart r =
  let
    newCurrent = newSection pos isRepeatStart
  in
    if not (isNullSection r.current) then
      r { sections = r.current : r.sections, current = newCurrent}
    else
      r { current = newCurrent }

-- return true if the section is devoid of any useful content
isNullSection :: Section -> Boolean
isNullSection s =
  s == nullSection

-- return true if the first (variant) ending is set
hasFirstEnding :: Section -> Boolean
hasFirstEnding s =
  isJust (unwrap s).firstEnding

-- set the isRepeated status of a section
setRepeated :: Section -> Section
setRepeated s =
  Section (unwrap s) { isRepeated = true }

-- set the end pisition of a section
setEndPos :: Int -> Section -> Section
setEndPos pos s =
  Section (unwrap s) { end = Just pos }

-- set the first repeat of a section
firstRepeat :: Int -> Section -> Section
firstRepeat pos s =
  Section (unwrap s) { firstEnding = Just pos }

-- | set the second repeat of a section
secondRepeat :: Int -> Section -> Section
secondRepeat pos s =
  Section (unwrap s) { secondEnding = Just pos }

-- start a new section
newSection :: Int -> Boolean -> Section
newSection pos isRepeated = Section
  { start : Just pos
  , firstEnding : Nothing
  , secondEnding : Nothing
  , end : Just 0
  , isRepeated : isRepeated
  , label : OtherPart         -- not used
  }

-- a 'null' section
nullSection :: Section
nullSection =
  newSection 0 false


-- | get the repeat number from the Volta 
-- | This is a terrible hack!
-- | WARNING this does not properly support complex volta lists 
getVoltaNumber :: Maybe Volta -> Maybe Int 
getVoltaNumber mvolta = 
  let
    toVoltaNumber :: Volta -> Int 
    toVoltaNumber volta = 
      case volta of 
        Volta v -> v 
        VoltaList vs -> 
          -- satisfy ourselves with just the first in the list
          NonEmpty.head (unwrap vs)
  in 
    map toVoltaNumber mvolta
    