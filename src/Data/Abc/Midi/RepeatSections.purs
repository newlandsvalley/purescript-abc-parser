module Data.Abc.Midi.RepeatSections
        ( Section
        , Sections
        , RepeatState
        , initialRepeatState
        , indexBar
        ) where

import Data.Abc (Repeat(..))
import Data.Generic (gEq, class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, (==), (&&), not)

-- | Handle any repeated sections of a tune
-- | Repeats are optional and can take the form:
-- |    |: ABC :|
-- |    |: ABC :: DEF :|
-- |    |: ABC |1 de :|2 fg |
-- | the very first repeat start marker is optional and often absent


-- | a section of the tune (possibly repeated)
newtype Section = Section
    { start :: Maybe Int
    , firstEnding :: Maybe Int
    , secondEnding :: Maybe Int
    , end :: Maybe Int
    , isRepeated :: Boolean
    }

derive instance newtypeSection :: Newtype Section _
derive instance genericSection :: Generic Section
instance eqSection :: Eq Section where  eq = gEq

-- | a set of sections
type Sections = List Section

-- | the current repeat state
type RepeatState =
    { current :: Section
    , sections :: Sections
    }

-- | initial repeats i.e. no repeats yet
initialRepeatState :: RepeatState
initialRepeatState =
  { current : nullSection, sections : Nil }

-- | index a bar by identifying any repeat markings and saving the marking against the bar number
indexBar :: (Maybe Int) -> (Maybe Repeat) -> Int -> RepeatState -> RepeatState
indexBar iteration repeat barNumber r =
  case (Tuple iteration repeat) of
    -- |1
    Tuple (Just 1) _ ->
      r { current = firstRepeat barNumber r.current}
    -- |2  or :|2
    Tuple (Just 2) _ ->
      r { current = secondRepeat barNumber r.current}
    -- |:
    Tuple _ (Just Begin) ->
      startSection barNumber r
    -- :|
    Tuple _ (Just End) ->
      endSection barNumber true r
    -- :|:  or ::
    Tuple _ (Just BeginAndEnd) ->
      endAndStartSection barNumber true true r
    _ ->
     r

-- | default sections i.e. no repeats yet
defaultSections :: RepeatState
defaultSections =
  { current : nullSection, sections : Nil }

-- | accumulate the last section and start a new section  -}
startSection :: Int -> RepeatState -> RepeatState
startSection pos r =
  -- a start implies an end of the last section
  endAndStartSection pos false true r

-- | end the section.  If there is a first repeat, keep it open, else accumulate it
-- | pos : the bar number marking the end of section
-- | isRepeatEnd : True if invoked with a known Repeat End marker in the bar line
endSection :: Int -> Boolean -> RepeatState -> RepeatState
endSection pos isRepeatEnd r =
  if (hasFirstEnding r.current) then
    let
      current = setEndPos pos r.current
    in
      r { current = current }
  else
     endAndStartSection pos isRepeatEnd false r

-- | end the current section, accumulate it and start a new section
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

-- | accumulate the current section into the full score and re-initialise it
accumulateSection :: Int -> Boolean -> RepeatState -> RepeatState
accumulateSection pos isRepeatStart r =
  let
    newCurrent = newSection pos isRepeatStart
  in
    if not (isNullSection r.current) then
      r { sections = r.current : r.sections, current = newCurrent}
  else
    r { current = newCurrent }

--  | return true if the section is devoid of any useful content
isNullSection :: Section -> Boolean
isNullSection s =
  s == nullSection

-- | return true if the first (variant) ending is set
hasFirstEnding :: Section -> Boolean
hasFirstEnding s =
  isJust (unwrap s).firstEnding

-- | set the isRepeated status of a section
setRepeated :: Section -> Section
setRepeated s =
  Section (unwrap s) { isRepeated = true }

-- | set the end pisition of a section
setEndPos :: Int -> Section -> Section
setEndPos pos s =
  Section (unwrap s) { end = Just pos }

-- | set the first repeat of a section
firstRepeat :: Int -> Section -> Section
firstRepeat pos s =
  Section (unwrap s) { firstEnding = Just pos }

-- | set the second repeat of a section
secondRepeat :: Int -> Section -> Section
secondRepeat pos s =
  Section (unwrap s) { secondEnding = Just pos }

-- | start a new section
newSection :: Int -> Boolean -> Section
newSection pos isRepeated = Section
  { start : Just pos
  , firstEnding : Nothing
  , secondEnding : Nothing
  , end : Just 0
  , isRepeated : isRepeated
  }

-- | a 'null' section
nullSection :: Section
nullSection =
  newSection 0 false
