module Data.Abc.Midi (toMidi) where

import Data.Abc.Accidentals as Accidentals
import Data.Midi as Midi
import Control.Monad.State (State, get, put, evalState)
import Data.Abc (AbcTune, AbcNote, Bar, Broken(..), Header(..), TuneBody, Repeat(..), BodyPart(..), MusicLine, Music(..), Mode(..), ModifiedKeySignature, TempoSignature, PitchClass(..))
import Data.Abc.Midi.RepeatSections (RepeatState, Section(..), Sections, initialRepeatState, indexBar, finalBar)
import Data.Abc.Notation (dotFactor, toMidiPitch, getKeySig)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, midiTempo, noteTicks, standardMidiTick)
import Data.Foldable (foldl)
import Data.List (List(..), (:), null, concatMap, filter, head, tail, reverse, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Rational (Rational, fromInt, rational)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (bind, pure, ($), (+), (-), (*), (<>), (>=), (<), (&&))

-- import Debug.Trace (trace, traceShow)

-- | Transform ABC into a MIDI recording.
toMidi :: AbcTune -> Midi.Recording
toMidi tune =
  do
    evalState (transformTune tune) (initialState tune)

-- | a bar of MIDI music
type MidiBar =
  { number :: Int                         -- sequential from zero
  , repeat :: Maybe Repeat                -- a repeat of some kind
  , iteration :: Maybe Int                -- an iteration marker  (|1  or |2 etc)
  , midiMessages :: List Midi.Message     -- the notes in the bar or any tempo changes
  }

-- | the state to thread through the computation
type TState =
    { modifiedKeySignature ::  ModifiedKeySignature    -- the current key signature
    , abcTempo ::  AbcTempo                            -- the current tempo
    , currentBar :: MidiBar                            -- the current bar being translated
    , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
                                                       -- any notes marked explicitly as accidentals in the current bar
    , lastNoteTied :: Boolean                          -- was the last note tied?
    , repeatState :: RepeatState                       -- the repeat state of the tune
    , rawTrack :: List MidiBar                         -- the growing list of completed bars
    }

type TransformationState =
  Tuple TState Midi.Recording

-- | a skeletal MIDI recording
skeletalRecording :: Midi.Recording
skeletalRecording =
  let
    header = Midi.Header
     { formatType : 0
     , trackCount : 1
     , ticksPerBeat : standardMidiTick
     }
  in Midi.Recording
    { header : header
    , tracks : Nil
    }

-- | The very first bar has a default tempo as the only message
initialBar :: Midi.Message -> MidiBar
initialBar initialMsg =
  { number : 0
  , repeat : Nothing
  , iteration : Nothing
  , midiMessages : (initialMsg : Nil)
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> Bar -> MidiBar
buildNewBar i abcBar =
  {  number : i
  ,  repeat : abcBar.repeat
  ,  iteration : abcBar.iteration
  ,  midiMessages : Nil
  }

-- | default to C Major (i.e. no accidental modifiers)
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental:  Nothing, mode: Major }, modifications: Nil }

-- the default MIDI volume (velocity)
defaultVolume :: Int
defaultVolume =  80

-- | this initial state is then threaded through the computation
-- | but will be altered when ABC headers are encountered
initialState :: AbcTune -> TransformationState
initialState tune =
  let
    abcTempo = getAbcTempo tune
    keySignature = fromMaybe defaultKey (getKeySig tune)
    initialMsg = midiTempoMsg abcTempo
    -- we must have a tempo indication at the very start
    -- startTrack = RawTrack $ (initialBar initialMsg) : Nil
  in
    Tuple { modifiedKeySignature: keySignature
          , abcTempo : abcTempo
          , currentBar : initialBar initialMsg
          , currentBarAccidentals : Accidentals.empty
          , lastNoteTied : false
          , repeatState : initialRepeatState
          , rawTrack : Nil
          } skeletalRecording

transformTune :: AbcTune -> State TransformationState Midi.Recording
transformTune tune =
  do
    -- we don't need to process the initial headers because
    -- they're already adopted in the initial state
    transformBody tune.body

transformBody :: TuneBody -> State TransformationState Midi.Recording
transformBody Nil =
  do
    finaliseMelody
transformBody (p : ps) =
  do
    transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TransformationState Midi.Recording
transformBodyPart bodyPart =
  case bodyPart of
    Score musicLine ->
      transformMusicLine musicLine
    BodyInfo header ->
      transformHeader header

transformMusicLine :: MusicLine -> State TransformationState Midi.Recording
transformMusicLine Nil =
  do
    tpl <- get
    pure $ snd tpl
transformMusicLine (l : ls) =
  do
    transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TransformationState Midi.Recording
transformMusic m =
  case m of
    Note abcNote ->
      updateState (addNoteToState false (rational 1 1)) abcNote

    Rest duration ->
      updateState addRestToState duration

    Tuplet signature tnotes ->
      updateState (addNotesToState false (rational signature.q signature.p)) tnotes

    Chord abcChord ->
      let
        arbitraryNote =
          { pitchClass : C
          , accidental : Nothing
          , octave : 0
          , duration : (fromInt 0)
          , tied : false
          }
        -- chordal notes should really be a non-empty list - the default is arbitrary
        first = fromMaybe arbitraryNote $
                   head abcChord.notes
        others = fromMaybe Nil $
                   tail abcChord.notes
      in
        do
          -- set the notes all to start at the same time
          updateState (addNotesToState true (rational 1 1)) abcChord.notes
          -- pace by adding a NoteOff for the first note
          updateState (addNoteOffToState abcChord.duration) first
          -- and terminate all the other notes with a NoteOff at 0 duration
          updateState (addNoteOffsToState (fromInt 0)) others

    BrokenRhythmPair note1 broken note2 ->
      case broken of
        LeftArrow i ->
          do
            updateState (addNoteToState false (brokenTempo i false)) note1
            updateState (addNoteToState false (brokenTempo i true)) note2
        RightArrow i ->
          do
            updateState (addNoteToState false (brokenTempo i true)) note1
            updateState (addNoteToState false (brokenTempo i false)) note2

    Barline bar ->
      -- transformBar bar
      updateState addBarToState bar

    Inline header ->
      transformHeader header

    _ ->
      do
        tpl <- get
        pure $ snd tpl

-- | add a bar tio the state.  index it and add it to the growing list of bars
addBarToState :: TState -> Bar -> TState
addBarToState tstate bar =
  -- the current bar held in state is empty so we coalesce
  if (isBarEmpty tstate.currentBar) then
    coalesceBar tstate bar
  -- it's not emmpty so we initialise the new bar
  else
    let
      currentBar = tstate.currentBar
      repeatState =
        indexBar currentBar.iteration currentBar.repeat currentBar.number tstate.repeatState
      -- ad this bar to the growing list of bars
      rawTrack =
        -- the current bar is not empty so we aggregate the new bar into the track
        currentBar : tstate.rawTrack
    in
      tstate { currentBar = buildNewBar (currentBar.number + 1) bar
             , currentBarAccidentals = Accidentals.empty
             , repeatState = repeatState
             , rawTrack = rawTrack
             }

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: TState -> Bar -> TState
coalesceBar tstate abcBar =
  let
    barRepeats = Tuple tstate.currentBar.repeat abcBar.repeat
    newRepeat = case barRepeats of
     Tuple (Just End) (Just Begin) ->
        Just BeginAndEnd
     Tuple ( Just x) _  ->
        Just x
     _ ->
        abcBar.repeat
    bar' = tstate.currentBar { repeat = newRepeat, iteration = abcBar.iteration }
  in
    tstate { currentBar = bar' }


-- | The unit note length and tempo headers affect tempo
-- | The key signature header affects pitch
-- | other headers have no effect
-- | but ABC allows headers to change mid-tune
transformHeader :: Header -> State TransformationState Midi.Recording
transformHeader h =
  case h of
    UnitNoteLength d ->
      updateState addUnitNoteLenToState d
    Key mks ->
      updateState addKeySigToState mks
    Tempo t ->
      updateState addTempoToState t
    _ ->
      do
        tpl <- get
        pure $ snd tpl

-- | a note is added to the current barAccidentals as a NoteOn NoteOff pair
-- | there are other implications for state - if the note has an explicit
-- | accidental, overriding the key then it is added to state because it
-- | influences other notes later in the bar
addNoteToState :: Boolean -> Rational -> TState-> AbcNote -> TState
addNoteToState chordal tempoModifier tstate abcNote =
  let
    barAccidentals = tstate.currentBarAccidentals
    -- if the last note was tied, we treat this note simply as a rest (zero pitch) in order to pace the tune properly
    pitch =
        toMidiPitch abcNote tstate.modifiedKeySignature barAccidentals
    ticks =
        noteTicks (abcNote.duration * tempoModifier)

    msgOn = midiNoteOn 0 pitch
    msgOff = midiNoteOff ticks pitch
    msgRest = midiNoteOn ticks 0

    bar = tstate.currentBar
    -- when we're adding a note, a 'normal note' is sounded by emitting
    -- a NoteOn followed by a NoteOff message.  However a note within a chord
    -- is started at the same time as it's accompanying notes and will only be switched
    -- off after each note is started (outside of this function)
    bar' =
      if chordal then
        bar { midiMessages = (msgOn : bar.midiMessages)}
      else if tstate.lastNoteTied then
        bar { midiMessages = (msgRest : bar.midiMessages)}
      else
        bar { midiMessages = (msgOff : msgOn : bar.midiMessages)}
    barAccidentals' = addNoteToBarAccidentals abcNote barAccidentals

  in
    tstate { currentBar = bar'
           , lastNoteTied = abcNote.tied
           , currentBarAccidentals = barAccidentals'
           }

-- | add a bunch of notes to the state
-- | chordal means that the notes form a chord and thus do not need
-- | NoteOff messages to be generated after each note
addNotesToState :: Boolean -> Rational -> TState-> List AbcNote -> TState
addNotesToState chordal tempoModifier tstate abcNotes =
  foldl (addNoteToState chordal tempoModifier) tstate abcNotes

-- possibly combine these next rwo routines

-- | add a NoteOff message which will terminate a chord
addNoteOffToState :: Rational -> TState -> AbcNote -> TState
addNoteOffToState duration tstate abcNote =
  let
    pitch =
      toMidiPitch abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
    msg = midiNoteOff (noteTicks duration) pitch
    bar' = tstate.currentBar { midiMessages = (msg : tstate.currentBar.midiMessages)}
  in
    tstate { currentBar = bar' }

-- | add a bunch of NoteOffs at the same duration
addNoteOffsToState :: Rational -> TState -> List AbcNote  -> TState
addNoteOffsToState duration tstate abcNotes =
  foldl (addNoteOffToState duration) tstate abcNotes

-- | add a pitchless NoteOn which indicates a rest
addRestToState :: TState-> Rational -> TState
addRestToState tstate duration =
  let
    -- a rest is a note without a pitch
    msg = midiNoteOn (noteTicks duration) 0
    bar' = tstate.currentBar { midiMessages = (msg : tstate.currentBar.midiMessages)}
  in
    tstate { currentBar = bar' }

-- | cater for a change in key signature
addKeySigToState :: TState-> ModifiedKeySignature -> TState
addKeySigToState tstate mks =
  tstate { modifiedKeySignature = mks }

-- | cater for a change in unit note length
addUnitNoteLenToState :: TState-> Rational -> TState
addUnitNoteLenToState tstate d =
  let
    abcTempo' = tstate.abcTempo { unitNoteLength = d}
    tempoMsg = midiTempoMsg abcTempo'
    bar' = tstate.currentBar { midiMessages = (tempoMsg : tstate.currentBar.midiMessages)}
  in
    tstate { abcTempo = abcTempo'
           , currentBar = bar' }

-- | cater for a change in unit note length
-- | this not only changes state but adds a change tempo message
addTempoToState :: TState-> TempoSignature -> TState
addTempoToState tstate tempoSig =
  let
    abcTempo' =
      tstate.abcTempo { tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
                      , bpm = tempoSig.bpm
                      }
    tempoMsg = midiTempoMsg abcTempo'
    bar' = tstate.currentBar { midiMessages = (tempoMsg : tstate.currentBar.midiMessages)}
  in
    tstate { abcTempo = abcTempo'
           , currentBar = bar' }

-- utility functions

-- | if the incoming note has an explicit accidental (overriding the key signature)
-- | then add it to the accidentals in force in the current bar
addNoteToBarAccidentals :: AbcNote -> Accidentals.Accidentals -> Accidentals.Accidentals
addNoteToBarAccidentals abcNote accs =
  case abcNote.accidental of
    Just acc  ->
      Accidentals.add abcNote.pitchClass acc accs
    _ ->
      accs

-- | a MIDI NoteOn message
midiNoteOn :: Int -> Int -> Midi.Message
midiNoteOn ticks pitch =
  Midi.Message ticks (Midi.NoteOn 0 pitch defaultVolume)

-- | a MIDI NoteOff message
midiNoteOff :: Int -> Int -> Midi.Message
midiNoteOff ticks pitch =
  Midi.Message ticks (Midi.NoteOff 0 pitch defaultVolume)

-- | a MIDI message to set the tempo
midiTempoMsg :: AbcTempo -> Midi.Message
midiTempoMsg abcTempo =
  Midi.Message 0 (Midi.Tempo $ midiTempo abcTempo)

-- | work out the broken rhythm tempo
brokenTempo :: Int -> Boolean -> Rational
brokenTempo i isUp =
  if isUp then
    (rational 1 1) + (dotFactor i)
  else
    (rational 1 1) - (dotFactor i)

-- | does the MIDI bar hold no notes (or any other MIDI messages)
isBarEmpty :: MidiBar -> Boolean
isBarEmpty mb =
    null mb.midiMessages

-- | generic function to update the State
-- | a is an ABC value
-- | f is a function that transforms the ABC value and adds it to the state
updateState :: forall a. (TState -> a -> TState ) -> a -> State TransformationState Midi.Recording
updateState f abc =
  do
    tpl <- get
    let
      recording = snd tpl
      tstate = fst tpl
      tstate' = f tstate abc
      tpl' = Tuple tstate' recording
    _ <- put tpl'
    pure recording

-- | move the final bar from state into the final track and then build the recording
-- | complete the RepeatState and then build the MIDI melody
finaliseMelody :: State TransformationState Midi.Recording
finaliseMelody =
  do
    tpl <- get
    let
      recording = snd tpl
      tstate = fst tpl
      currentBar = tstate.currentBar
      -- index the final bar and finalise the repear state
      repeatState =
        finalBar currentBar.iteration currentBar.repeat currentBar.number tstate.repeatState
      -- ensure we incorporate the very last bar
      tstate' = tstate { rawTrack = tstate.currentBar : tstate.rawTrack
                       , repeatState = repeatState }
      track = buildRepeatedMelody tstate'.rawTrack tstate'.repeatState.sections
      recording' :: Midi.Recording
      recording' = Midi.Recording (unwrap recording) { tracks = singleton $ track }
      tpl' = Tuple tstate' recording'
    _ <- put tpl'
    pure recording'

-- the following functions deal with interpreting repeated sections

-- | accumulate the MIDI messages from the List of bars
accumulateMessages :: List MidiBar -> List Midi.Message
accumulateMessages mbs =
  reverse $ concatMap _.midiMessages mbs

-- | turn a list of bars (with repeats removed) into a track
-- | temporary measure until we integrate repeats
buildSimpleTrack :: List MidiBar -> Midi.Track
buildSimpleTrack mbs =
    Midi.Track $ accumulateMessages mbs

-- | select a subset of MIDI bars
barSelector :: Int -> Int -> MidiBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

-- | build the notes from a subsection of the track
trackSlice :: Int -> Int -> List MidiBar -> List Midi.Message
trackSlice start finish mbs =
  accumulateMessages $ filter (barSelector start finish) mbs

-- | take two variant slices of a melody line between start and finish
-- |    taking account of first repeat and second repeat sections
variantSlice :: Int -> Int -> Int -> Int -> List MidiBar-> List Midi.Message
variantSlice start firstRepeat secondRepeat end mbs =
  let
    -- save the section of the tune we're interested in
    section = filter (barSelector start end) mbs
    -- |: ..... |2
    firstSection = trackSlice start secondRepeat section
    -- |: .... |1  + |2 ..... :|
    secondSection = trackSlice start firstRepeat section <> trackSlice secondRepeat end section
  in
    firstSection <> secondSection

-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection ::  List MidiBar -> List Midi.Message -> Section -> List Midi.Message
repeatedSection mbs acc (Section { start: Just a, firstEnding: Just b, secondEnding : Just c, end: Just d, isRepeated : _ }) =
  (variantSlice a b c d mbs) <> acc
repeatedSection mbs acc (Section { start: Just a, firstEnding: _, secondEnding : _, end: Just d, isRepeated : false }) =
  (trackSlice a d mbs) <> acc
repeatedSection mbs acc (Section { start: Just a, firstEnding: _, secondEnding : _, end: Just d, isRepeated : true }) =
  (trackSlice a d mbs) <> (trackSlice a d mbs) <> acc
repeatedSection mbs acc _ =
  acc

-- | build any repeated section into an extended melody with all repeats realised -}
buildRepeatedMelody :: List MidiBar -> Sections -> Midi.Track
buildRepeatedMelody mbs sections =
   -- trace "Sections" \_ ->
   -- traceShow sections \_ ->
  if (null sections) then
    Midi.Track Nil
  else
    Midi.Track $ foldl (repeatedSection mbs) Nil sections

-- temp Debug
{-
showBar :: MidiBar -> String
showBar mb =
  "barnum: " <> show (mb.number) <>  " message count: " <> show (length mb.midiMessages) <> " repeat " <>  show mb.repeat <>" "

showBars :: List MidiBar -> String
showBars mbs =
  let
    f :: MidiBar -> String -> String
    f mb acc = acc <> showBar mb
  in
   foldr f "" mbs
-}
