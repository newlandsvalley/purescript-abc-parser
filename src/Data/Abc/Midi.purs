module Data.Abc.Midi (toMidi) where

import Data.Abc.Accidentals as Accidentals
import Data.Midi as Midi
import Control.Monad.State (State, get, put, evalState)
import Data.Abc (AbcTune, AbcNote, Bar, Broken(..), Header(..), TuneBody, Repeat(..), BodyPart(..), MusicLine, Music(..), Mode(..), ModifiedKeySignature, TempoSignature, PitchClass(..))
import Data.Abc.Notation (dotFactor, toMidiPitch, getKeySig)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, midiTempo, noteTicks, standardMidiTick)
import Data.Foldable (foldl)
import Data.Generic (gShow, class Generic)
import Data.List (List(..), (:), null, concatMap, reverse, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Rational (Rational, fromInt, rational)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Show, bind, pure, ($), (+), (-), (*))
import Data.Abc.Midi.RepeatSections (RepeatState, initialRepeatState, indexBar)

-- | Transform ABC into a MIDI recording.
toMidi :: AbcTune -> Midi.Recording
toMidi tune =
  do
    evalState (transformTune tune) (initialState tune)

-- | a bar of MIDI music
newtype MidiBar = MidiBar
  { number :: Int                         -- sequential from zero
  , repeat :: Maybe Repeat                -- a repeat of some kind
  , iteration :: Maybe Int                -- an iteration marker  (|1  or |2 etc)
  , midiMessages :: List Midi.Message     -- the notes in the bar or any tempo changes
  }

derive instance newtypeMidiBar :: Newtype MidiBar _
derive instance genericMidiBar :: Generic MidiBar
instance showMidiBar :: Show MidiBar where
  show = gShow

type TState =
    { modifiedKeySignature ::  ModifiedKeySignature    -- the current key signature
    , abcTempo ::  AbcTempo                            -- the current tempo
    , currentBar :: MidiBar                            -- the current bar being translated
    , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
                                                       -- any notes marked explicitly as accidentals in the current bar
    , lastNoteTied :: Boolean                          -- was the last note tied?
    , repeatState :: RepeatState                       -- the repeat state of the tune
    , rawTrack :: List MidiBar
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
initialBar initialMsg = MidiBar
  { number : 0
  , repeat : Nothing
  , iteration : Nothing
  , midiMessages : (initialMsg : Nil)
  }

-- | generate a new bar
newBar :: Int -> MidiBar
newBar n = MidiBar
  { number : n
  , repeat : Nothing
  , iteration : Nothing
  , midiMessages : Nil
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
          , currentBar : newBar 0
          , currentBarAccidentals : Accidentals.empty
          , lastNoteTied : false
          , repeatState : initialRepeatState
          , rawTrack : ((initialBar initialMsg) : Nil )
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
      do
        -- set the notes all to start at the same time
        updateState (addNotesToState true (rational 1 1)) abcChord.notes
        -- pace by adding a NoteOff
        updateState addNoteOffToState abcChord.duration

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
      transformBar bar

    Inline header ->
      transformHeader header

    _ ->
      do
        tpl <- get
        pure $ snd tpl

-- | a bar causes the currently built bar to be moved to the track
-- | and we then start a new bar
transformBar :: Bar -> State TransformationState Midi.Recording
transformBar bar =
  do
    tpl <- get
    let
      tstate = fst tpl
      recording = snd tpl
      currentBar :: MidiBar
      currentBar = tstate.currentBar

      tstate' =
        -- the current bar held in state is empty so we coalesce
        if (isBarEmpty currentBar) then
          coalesceBar tstate bar
        -- it's not emmpty so we initialise the new bar and index the
        -- last bar which keeps track of repeated sections in the melody
        else
          let
            lastBar = unwrap (currentBar)
            repeatState =
              indexBar lastBar.iteration lastBar.repeat lastBar.number tstate.repeatState
            rawTrack :: List MidiBar
            rawTrack =
              -- the current bar is not empty so we aggregate the new bar into the track
              currentBar : tstate.rawTrack
          in
            tstate { currentBar = newBar ((unwrap currentBar).number + 1)
                   , currentBarAccidentals = Accidentals.empty
                   , repeatState = repeatState
                   , rawTrack = rawTrack
                   }

      tpl' = Tuple tstate' recording
    _ <- put tpl'
    pure recording

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: TState -> Bar -> TState
coalesceBar tstate abcBar =
  let
    currentBar = unwrap tstate.currentBar
    barRepeats = Tuple currentBar.repeat abcBar.repeat
    newRepeat = case barRepeats of
     Tuple (Just End) (Just Begin) ->
        Just BeginAndEnd
     Tuple ( Just x) _  ->
        Just x
     _ ->
        abcBar.repeat
    bar' = currentBar { repeat = newRepeat, iteration = abcBar.iteration }
  in
    tstate { currentBar = MidiBar bar' }

buildNewBar :: Int -> Bar -> MidiBar
buildNewBar i abcBar =
  MidiBar
    {  number : i
    ,  repeat : abcBar.repeat
    ,  iteration : abcBar.iteration
    ,  midiMessages : Nil
    }

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

    bar = unwrap tstate.currentBar
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
    tstate { currentBar = MidiBar bar'
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
addNoteOffToState :: TState -> Rational -> TState
addNoteOffToState tstate duration =
  let
    -- we take 0 to represent any arbitrary note in the chord
    msg = midiNoteOff (noteTicks duration) 0
    bar = unwrap tstate.currentBar
    bar' = bar { midiMessages = (msg : bar.midiMessages)}
  in
    tstate { currentBar = MidiBar bar' }

-- | add a pitchless NoteOn message which indicates a rest
addRestToState :: TState-> Rational -> TState
addRestToState tstate duration =
  let
    -- a rest is a note without a pitch
    msg = midiNoteOn (noteTicks duration) 0
    bar = unwrap tstate.currentBar
    bar' = bar { midiMessages = (msg : bar.midiMessages)}
  in
    tstate { currentBar = MidiBar bar' }

-- | cater for a change in key signature
addKeySigToState :: TState-> ModifiedKeySignature -> TState
addKeySigToState tstate mks =
  tstate { modifiedKeySignature = mks }

-- | cater for a change in unit note length
addUnitNoteLenToState :: TState-> Rational -> TState
addUnitNoteLenToState tstate d =
  let
    abcTempo = tstate.abcTempo
    abcTempo' = abcTempo { unitNoteLength = d}
    tempoMsg = midiTempoMsg abcTempo'
    bar = unwrap tstate.currentBar
    bar' = bar { midiMessages = (tempoMsg : bar.midiMessages)}
  in
    tstate { abcTempo = abcTempo'
           , currentBar = MidiBar bar' }

-- | cater for a change in unit note length
-- | this not only changes state but adds a change tempo message
addTempoToState :: TState-> TempoSignature -> TState
addTempoToState tstate tempoSig =
  let
    abcTempo = tstate.abcTempo
    abcTempo' = abcTempo { tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
                         , bpm = tempoSig.bpm
                         }
    tempoMsg = midiTempoMsg abcTempo'
    bar = unwrap tstate.currentBar
    bar' = bar { midiMessages = (tempoMsg : bar.midiMessages)}
  in
    tstate { abcTempo = abcTempo'
           , currentBar = MidiBar bar' }

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
    null (unwrap mb).midiMessages

-- concatMap :: forall a b. (a -> List b) -> List a -> List b

-- | turn a list of bars into a track
-- | temporary measure until we integrate repeats
buildTrack :: List MidiBar -> Midi.Track
buildTrack rt =
  let
    f :: MidiBar -> List Midi.Message
    f b  = (unwrap b).midiMessages
  in
    Midi.Track $ reverse $ concatMap f rt

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

-- | move the final bar from state into the final track
finaliseMelody :: State TransformationState Midi.Recording
finaliseMelody =
  do
    tpl <- get
    let
      recording = snd tpl
      tstate = fst tpl
      tstate' = tstate { rawTrack = tstate.currentBar : tstate.rawTrack }
      track = buildTrack (tstate'.rawTrack)
      recording' :: Midi.Recording
      recording' = Midi.Recording (unwrap recording) { tracks = singleton $ track }
      tpl' = Tuple tstate' recording'
    _ <- put tpl'
    pure recording'
