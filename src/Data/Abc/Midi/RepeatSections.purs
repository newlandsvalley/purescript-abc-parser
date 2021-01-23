-- | Handle any repeated sections when interpreting an ABC tune
-- | Repeats are optional and can take the form:
-- |    |: ABC :|
-- |    |:: ABC :|
-- |    |: ABC :: DEF :|
-- |    |: ABC |1 de :|2 fg |
-- |    |: ABC |1,3 de :|2,4 fg |
-- | the very first repeat start marker is optional and often absent
module Data.Abc.Midi.RepeatSections
        ( initialRepeatState
        , indexBar
        , finalBar
        ) where

import Data.Abc (Volta(..))
import Data.Abc.Midi.Types (MidiBar)
import Data.Abc.Repeats.Types (RepeatState, Section)
import Data.Abc.Repeats.Variant (setVariantList, setVariantOf)
import Data.Abc.Repeats.Section (isDeadSection, isUnrepeated, hasFirstEnding, newSection, 
         nullSection, setEndPos, setMissingRepeatCount)
import Data.Array (fromFoldable)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Prelude ((==), (>), (<=), (&&), (-), ($), map, not)

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
    -- |1 or |2 etc
    Just (Volta n), _ , _ ->    
      r { current = setVariantOf (toOffsetZero n) mb.number r.current}
    -- | 1,2 etc 
    Just (VoltaList vs), _ , _ ->
      let 
        vsArray = map toOffsetZero $ fromFoldable vs
      in
        r { current = setVariantList vsArray mb.number r.current}
    Nothing,  ends,  starts ->    
      if (ends > 0 && starts > 0) then
        endAndStartSection mb.number true starts r
      else if (ends > 0 && starts <= 0) then
        endSection mb.number true r
      else if (ends <= 0 && starts > 0) then
        startSection mb.number starts r
      else 
        r

{-| accumulate any residual current state from the final bar in the tune -}
finalBar :: MidiBar -> RepeatState -> RepeatState
finalBar mb r =
  let
    isRepeatEnd = mb.endRepeats > 0 
    repeatState = endSection mb.number isRepeatEnd r
  in
    if not (isDeadSection r.current) then
      accumulateSection mb.number 0 repeatState
    else
      repeatState

-- default sections i.e. no repeats yet
defaultSections :: RepeatState
defaultSections =
  { current : nullSection, sections : Nil, intro : [] }

-- accumulate the last section and start a new section  -}
startSection :: Int -> Int -> RepeatState -> RepeatState
startSection pos repeatStartCount r =
  -- a start implies an end of the last section
  endAndStartSection pos false repeatStartCount r

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
     endAndStartSection pos isRepeatEnd 0 r

-- end the current section, accumulate it and start a new section
endAndStartSection :: Int -> Boolean -> Int -> RepeatState -> RepeatState
endAndStartSection endPos isRepeatEnd repeatStartCount r =
  let
    -- cater for the situation where the ABC marks the first section of the tune as repeated solely by use
    -- of the End Repeat marker with no such explicit marker at the start of the section - it is implied as the tune start
    current :: Section
    current =
      if isRepeatEnd 
         && (unwrap r.current).start == Just 0 
         && (isUnrepeated r.current)  then
          setMissingRepeatCount r.current
      else
        r.current
    -- now set the end position from the bar number position
    current' = setEndPos endPos current
    -- set the new current into the state
    endState :: RepeatState
    endState = r { current = current' }
  in
    accumulateSection endPos repeatStartCount endState

-- accumulate the current section into the full score and re-initialise it
accumulateSection :: Int -> Int -> RepeatState -> RepeatState
accumulateSection pos repeatStartCount r =
  let
    newCurrent = newSection pos repeatStartCount
  in
    if not (isDeadSection r.current) then
      r { sections = r.current : r.sections, current = newCurrent}
    else
      r { current = newCurrent }

-- | volta repeat markers are wrt offset 1 - reduce to 0
toOffsetZero :: Int -> Int 
toOffsetZero i =
  if i <= 0 then 0 else i -1  
