-- | Conversion of an ABC tune to MIDI.
module Data.Abc.Midi
  ( module Data.Abc.Midi.Pitch
  , toMidi
  , toMidiAtBpm
  , toMidiRecording
  , toMidiRecordingAtBpm
  ) where

import Control.Monad.State (State, get, put, execState, modify_)
import Data.Abc
  ( AbcTune
  , AbcNote
  , Bar
  , RestOrNote
  , Accidental(..)
  , BarLine
  , Header(..)
  , TuneBody
  , BodyPart(..)
  , Grace
  , GraceableNote
  , MusicLine
  , Music(..)
  , ModifiedKeySignature
  , TempoSignature
  )
import Data.Abc.Accidentals as Accidentals
import Data.Abc.KeySignature (defaultKey, getKeySig)
import Data.Abc.Midi.Pitch (MidiPitch, toMidiPitch) 
import Data.Abc.Midi.Types (MidiBar, MidiBars)
import Data.Abc.Midi.RepeatSections (initialRepeatState, indexBar, finalBar)
import Data.Abc.Normaliser (normalise)
import Data.Abc.Repeats.Types (RepeatState, Section(..), Sections)
import Data.Abc.Repeats.Variant (activeVariants, findEndingPosition, variantPositionOf, variantCount)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, midiTempo, noteTicks, setBpm, standardMidiTick)
import Data.Either (Either(..))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl, foldM)
import Data.Unfoldable (replicate)
import Data.Array as Array
import Data.List (List(..), (:), null, concat, concatMap, foldr, filter, reverse, singleton)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, length, tail, toList) as Nel
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi as Midi
import Data.Midi.Generate (recording) as Generate
import Data.Newtype (unwrap)
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, const, identity, map, pure, unit, ($), (&&), (*), (+), (-), (<), (>), (<>), (>=))

-- | Transform the ABC into raw MIDI
toMidi :: AbcTune -> List Midi.Byte 
toMidi tune = 
  Generate.recording $ toMidiRecording tune

-- | Transform the ABC into raw MIDI but at the modified tempo
-- | defined by the new BPM (beats per minute)
toMidiAtBpm :: AbcTune -> Int -> List Midi.Byte 
toMidiAtBpm tune bpm = 
  Generate.recording $ toMidiRecordingAtBpm tune bpm

-- | Transform ABC into a MIDI recording.
toMidiRecording :: AbcTune -> Midi.Recording
toMidiRecording tune =
  let
    -- we normalise the tune to replace all broken rhythm pairs with normal notes or rests
    tstate = execState (transformTune $ normalise tune) (initialState tune)
  in
    makeRecording tstate

-- | Transform ABC into a MIDI recording but at the modified tempo
-- | defined by the new BPM (beats per minute)
toMidiRecordingAtBpm :: AbcTune -> Int -> Midi.Recording
toMidiRecordingAtBpm originalTune bpm =
  let
    -- we normalise the tune to replace all broken rhythm pairs with normal notes or rests
    tune = setBpm bpm $ normalise originalTune
    tstate = execState (transformTune tune) (initialState tune)
  in
    makeRecording tstate

-- | the state to thread through the computation
type TState =
  { modifiedKeySignature :: ModifiedKeySignature -- the current key signature
  , abcTempo :: AbcTempo -- the current tempo
  , currentBar :: MidiBar -- the current bar being translated
  , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
  -- any notes marked explicitly as accidentals in the current bar
  , lastNoteTied :: Maybe AbcNote -- the last note, if it was tied?
  , repeatState :: RepeatState -- the repeat state of the tune
  , rawTrack :: List MidiBar -- the growing list of completed bars
  }

-- | the fraction  of the duration of a note that is 'stolen' by any
-- | preceding grace note that it has
graceFraction :: Rational
graceFraction =
  (1 % 10)  

-- | The very first bar has a default tempo as the only message
initialBar :: Midi.Message -> MidiBar
initialBar initialMsg =
  { number: 0
  , endRepeats: 0
  , startRepeats: 0
  , iteration: Nothing
  , midiMessages: initialMsg : Nil
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarLine -> MidiBar
buildNewBar i barLine =
  { number: i
  , endRepeats: barLine.endRepeats
  , startRepeats: barLine.startRepeats
  , iteration: barLine.iteration
  , midiMessages: Nil
  }

-- the default MIDI volume (velocity)
defaultVolume :: Int
defaultVolume = 80

-- | this initial state is then threaded through the computation
-- | but will be altered when ABC headers are encountered
initialState :: AbcTune -> TState
initialState tune =
  let
    abcTempo = getAbcTempo tune

    keySignature :: ModifiedKeySignature
    keySignature = fromMaybe defaultKey (getKeySig tune)
    initialMsg = midiTempoMsg abcTempo
  -- we must have a tempo indication at the very start
  -- startTrack = RawTrack $ (initialBar initialMsg) : Nil
  in
    { modifiedKeySignature: keySignature
    , abcTempo: abcTempo
    , currentBar: initialBar initialMsg
    , currentBarAccidentals: Accidentals.empty
    , lastNoteTied: Nothing
    , repeatState: initialRepeatState
    , rawTrack: Nil
    }

transformTune :: AbcTune -> State TState Unit
transformTune tune =
  do
    -- we don't need to process the initial headers because
    -- they're already adopted in the initial state
    transformBody tune.body

transformBody :: TuneBody -> State TState Unit
transformBody Nil =
  do
    finaliseMelody
transformBody (p : ps) =
  do
    _ <- transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TState Unit
transformBodyPart bodyPart =
  case bodyPart of
    Score bars ->
      transformBarList bars
    BodyInfo header ->
      transformHeader header

transformBarList :: List Bar -> State TState Unit
transformBarList Nil =
  pure unit
transformBarList (b : bs) =
  do
    _ <- transformBar b
    transformBarList bs

transformBar :: Bar -> State TState Unit
transformBar bar =
  do
    _ <- handleBar bar.startLine
    transformMusicLine bar.music

transformMusicLine :: MusicLine -> State TState Unit
transformMusicLine Nil =
  pure unit
transformMusicLine (l : ls) =
  do
    _ <- transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TState Unit
transformMusic m =
  case m of
    Note graceableNote ->
      handleGraceableNote false (1 % 1) graceableNote

    Rest r ->
      handleRest r.duration

    Tuplet t ->
      handleTupletContents t.maybeGrace (t.signature.q % t.signature.p) t.restsOrNotes

    Chord abcChord ->
      let
        first = Nel.head abcChord.notes
        others = Nel.tail abcChord.notes
        -- we'll pace the chord from the duration of the first note it contains,
        -- modified by the overall chord duration
        duration = abcChord.duration * first.duration
      in
        do
          -- set the notes all to start at the same time
          _ <- handleAddNotes true (1 % 1) (Nel.toList abcChord.notes)
          -- pace by adding a NoteOff for the overall duration
          _ <- handleNoteOff duration first
          -- and terminate all the other notes with a NoteOff at 0 duration
          handleNoteOffs (fromInt 0) others

    Inline header ->
      transformHeader header

    _ -> do
      -- this includes BrokenRhythmPair which is replaced by a pair of individual notes or rests
      -- assuming normalise is called prior to generating the MIDI
      pure unit

-- | add a bar to the state.  index it and add it to the growing list of bars
handleBar :: BarLine -> State TState Unit
handleBar barLine = do
  tstate <- get
  -- the current bar held in state is empty so we coalesce
  if (isBarEmpty tstate.currentBar) then
    coalesceBar barLine
  -- it's not emmpty so we initialise the new bar
  else do
    let
      currentBar = tstate.currentBar
      repeatState =
        indexBar currentBar tstate.repeatState
      -- ad this bar to the growing list of bars
      rawTrack =
        -- the current bar is not empty so we aggregate the new bar into the track
        currentBar : tstate.rawTrack
    put
      tstate
        { currentBar = buildNewBar (currentBar.number + 1) barLine
        , currentBarAccidentals = Accidentals.empty
        , repeatState = repeatState
        , rawTrack = rawTrack
        }

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: BarLine -> State TState Unit
coalesceBar barLine = do
  tstate <- get
  let
    endRepeats = tstate.currentBar.endRepeats + barLine.endRepeats
    startRepeats = tstate.currentBar.startRepeats + barLine.startRepeats
    bar' = tstate.currentBar
      { endRepeats = endRepeats
      , startRepeats = startRepeats
      , iteration = barLine.iteration
      }
  put
    tstate { currentBar = bar' }

-- | The unit note length and tempo headers affect tempo
-- | The key signature header affects pitch
-- | other headers have no effect
-- | but ABC allows headers to change mid-tune
transformHeader :: Header -> State TState Unit
transformHeader h =
  case h of
    UnitNoteLength d ->
      modify_ $ addUnitNoteLenToState d
    Key mks ->
      modify_ $ addKeySigToState mks
    Tempo t ->
      modify_ $ addTempoToState t
    _ -> do
      pure unit

handleGraceableNote :: Boolean -> Rational -> GraceableNote -> State TState Unit
handleGraceableNote chordal tempoModifier graceableNote =
  handleNote chordal tempoModifier graceableNote.maybeGrace graceableNote.abcNote

-- | a note is added to the current barAccidentals as a NoteOn NoteOff pair
-- | possible preceded by its grace notes (if they exist)
-- | there are other implications for state - if the note has an explicit
-- | accidental, overriding the key then it is added to state because it
-- | influences other notes later in the bar
handleNote :: Boolean -> Rational -> Maybe Grace -> AbcNote -> State TState Unit
handleNote chordal tempoModifier maybeGrace abcNote = do
  tstate <- get
  let
    Tuple msgs newTie =
      if chordal then
        processChordalNote tempoModifier tstate abcNote
      else
        processNoteWithTie tempoModifier maybeGrace tstate abcNote
    barAccidentals =
      addNoteToBarAccidentals abcNote tstate.currentBarAccidentals
  put
    tstate
      { currentBar = tstate.currentBar { midiMessages = msgs }
      , lastNoteTied = newTie
      , currentBarAccidentals = barAccidentals
      }

-- | tuplets can now contain rests
handleRestOrNote :: Boolean -> Rational -> RestOrNote -> State TState Unit
handleRestOrNote chordal tempoModifier restOrNote =
  case restOrNote of
    Left r ->
      --  modifiy the rest duration by the tempo modifier
      handleRest (r.duration * tempoModifier)
    Right n ->
      handleGraceableNote chordal tempoModifier n

-- | process the incoming  note that is part of a chord.
-- | Here, ties and grace notes preceding the note
-- | are not supported
processChordalNote :: Rational -> TState -> AbcNote -> Tuple (List Midi.Message) (Maybe AbcNote)
processChordalNote tempoModifier tstate abcNote =
  let
    msgOn = emitNoteOn tstate abcNote
  in
    case tstate.lastNoteTied of
      Just lastNote ->
        -- we don't support ties or grace notes into chords
        -- just emit the tied note before the chordal note
        let
          tiedNotes = emitNoteOnOff tempoModifier tstate lastNote
        in
          Tuple (msgOn : (tiedNotes <> tstate.currentBar.midiMessages)) Nothing
      _ ->
        Tuple (msgOn : tstate.currentBar.midiMessages) Nothing

-- | process the incoming note, accounting for the fact that the previous note may have been tied.
-- |
-- | if it was tied, then we simply coalesce the notes by adding their durations.  If the incoming note
-- | is tied, then the (possibly combined) note is saved as the 'lastNoteTied' so that the whole
-- | process will begin again at the next note.  If not tied, then the (possibly combined) note
-- | is written into the current MIDI bar
processNoteWithTie :: Rational -> Maybe Grace -> TState -> AbcNote -> Tuple (List Midi.Message) (Maybe AbcNote)
processNoteWithTie tempoModifier maybeGrace tstate abcNote =
  case tstate.lastNoteTied of
    Just lastNote ->
      -- if the last note was tied, we can't have grace notes on this new note
      -- so we just ignore them
      let
        combinedAbcNote = abcNote { duration = abcNote.duration + lastNote.duration }
      in
        if abcNote.tied then
          -- save the combined note in lastNoteTied
          Tuple tstate.currentBar.midiMessages (Just combinedAbcNote)
        else
          -- emit the note and set lastNoteTied to Nothing
          let
            notes = emitNoteOnOff tempoModifier tstate combinedAbcNote
          in
            Tuple (notes <> tstate.currentBar.midiMessages) Nothing
    _ ->
      let
        -- the note is perhaps graced, in which case we have to reduce its duration
        gracedNote = curtailedGracedNote maybeGrace abcNote
        -- and we need to calculate the exact duration of each AbcNote in the
        -- list of ABC Notes
        graceAbcNotes =
          case maybeGrace of
            Just grace ->
              map (individualGraceNote abcNote) $ Nel.toList grace.notes
            _ ->
              Nil
        -- and emit the MIDI notes for each grace note
        graceNotes = emitNotesOnOff tempoModifier tstate graceAbcNotes
      in
        if gracedNote.tied then
          -- set lastNoteTied
          Tuple (graceNotes <> tstate.currentBar.midiMessages) (Just gracedNote)
        else
          let
            notes = emitNoteOnOff tempoModifier tstate gracedNote
          in
            -- write out the note to the current MIDI bar
            Tuple (notes <> graceNotes <> tstate.currentBar.midiMessages) Nothing

-- | emit a standard note which is represented by a NoteOn Message
-- | followed by a NoteOff for the same pitch with the required delay
emitNoteOnOff :: Rational -> TState -> AbcNote -> List Midi.Message
emitNoteOnOff tempoModifier tstate abcNote =
  let
    pitch =
      toMidiPitch tstate.modifiedKeySignature tstate.currentBarAccidentals abcNote
    ticks =
      noteTicks (abcNote.duration * tempoModifier)
  in
    ((midiNoteOff ticks pitch) : (midiNoteOn 0 pitch) : Nil)

-- | emit a sequence of on off MIDI messages from a sequence of ABC notes
-- | used for grace notes
emitNotesOnOff :: Rational -> TState -> List AbcNote -> List Midi.Message
emitNotesOnOff tempoModifier tstate abcNotes =
  let
    f :: AbcNote -> List Midi.Message -> List Midi.Message
    f n acc =
      acc <> (emitNoteOnOff tempoModifier tstate n)
  in
    foldr f Nil abcNotes

-- | emit just a NoteOn message (this is restricted to chords)
emitNoteOn :: TState -> AbcNote -> Midi.Message
emitNoteOn tstate abcNote =
  let
    pitch =
      toMidiPitch tstate.modifiedKeySignature tstate.currentBarAccidentals abcNote
  in
    midiNoteOn 0 pitch

-- | chordal means that the notes form a chord and thus do not need
-- | NoteOff messages to be generated after each note
handleAddNotes :: Boolean -> Rational -> List AbcNote -> State TState Unit
handleAddNotes chordal tempoModifier abcNotes = do
  foldM (const $ handleNote chordal tempoModifier Nothing) unit abcNotes

-- | Add the contents of a tuplet to state.  This is an optional grace note
-- | plus a list of either rests or notes.
-- | tempo modifier is the modification of the tempo indicate donly by the
-- | tuplet signature (e.g. 3 notes in the time of two)

handleTupletContents :: Maybe Grace -> Rational -> NonEmptyList RestOrNote -> State TState Unit
handleTupletContents mGrace tempoModifier restsOrNotes = do
  let
    -- move the grace notes from external to internal
    gracedRestsOrNotes = gracifyFirstNote mGrace restsOrNotes
  foldM (const $ handleRestOrNote false tempoModifier) unit gracedRestsOrNotes

-- | add a NoteOff message which will terminate a chord
handleNoteOff :: Rational -> AbcNote -> State TState Unit
handleNoteOff duration abcNote = do
  tstate <- get
  let
    pitch =
      toMidiPitch tstate.modifiedKeySignature tstate.currentBarAccidentals abcNote
    msg = midiNoteOff (noteTicks duration) pitch
    bar' = tstate.currentBar { midiMessages = (msg : tstate.currentBar.midiMessages) }
  put
    tstate { currentBar = bar' }

-- | add a bunch of NoteOffs at the same duration
handleNoteOffs :: Rational -> List AbcNote -> State TState Unit
handleNoteOffs duration abcNotes =
  foldM (const $ handleNoteOff duration) unit abcNotes

-- | add a pitchless NoteOn which indicates a rest
handleRest :: Rational -> State TState Unit
handleRest duration = do
  tstate <- get
  let
    -- a rest is a note without a pitch
    msg = midiNoteOn (noteTicks duration) 0
    bar' = tstate.currentBar { midiMessages = (msg : tstate.currentBar.midiMessages) }
  put
    tstate { currentBar = bar' }

-- | cater for a change in key signature
addKeySigToState :: ModifiedKeySignature -> TState -> TState
addKeySigToState mks tstate =
  tstate { modifiedKeySignature = mks }

-- | cater for a change in unit note length
addUnitNoteLenToState :: Rational -> TState -> TState
addUnitNoteLenToState d tstate =
  let
    abcTempo' = tstate.abcTempo { unitNoteLength = d }
    tempoMsg = midiTempoMsg abcTempo'
    bar' = tstate.currentBar { midiMessages = (tempoMsg : tstate.currentBar.midiMessages) }
  in
    tstate
      { abcTempo = abcTempo'
      , currentBar = bar'
      }

-- | cater for a change in unit note length
-- | this not only changes state but adds a change tempo message
addTempoToState :: TempoSignature -> TState -> TState
addTempoToState tempoSig tstate =
  let
    abcTempo' =
      tstate.abcTempo
        { tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
        , bpm = tempoSig.bpm
        }
    tempoMsg = midiTempoMsg abcTempo'
    bar' = tstate.currentBar { midiMessages = (tempoMsg : tstate.currentBar.midiMessages) }
  in
    tstate
      { abcTempo = abcTempo'
      , currentBar = bar'
      }

-- utility functions

-- | if the incoming note has an explicit accidental (overriding the key signature)
-- | then add it to the accidentals in force in the current bar
addNoteToBarAccidentals :: AbcNote -> Accidentals.Accidentals -> Accidentals.Accidentals
addNoteToBarAccidentals abcNote accs =
  case abcNote.accidental of
    Implicit ->
      accs
    acc ->
      Accidentals.add abcNote.pitchClass acc accs

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

-- | does the MIDI bar hold no notes (or any other MIDI messages)
isBarEmpty :: MidiBar -> Boolean
isBarEmpty mb =
  null mb.midiMessages

-- | move the final bar from state into the final track and then build the recording
-- | complete the RepeatState and then build the MIDI melody
finaliseMelody :: State TState Unit
finaliseMelody = do
  tstate <- get
  let
    currentBar = tstate.currentBar
    -- index the final bar and finalise the repear state
    repeatState =
      finalBar currentBar tstate.repeatState
    -- ensure we incorporate the very last bar
    newState =
      tstate
        { rawTrack = tstate.currentBar : tstate.rawTrack
        , repeatState = repeatState
        }
  put newState

makeRecording :: TState -> Midi.Recording
makeRecording tstate =
  let
    track = buildRepeatedMelody tstate.rawTrack tstate.repeatState.sections
    header = Midi.Header
      { formatType: 0
      , trackCount: 1
      , ticksPerBeat: standardMidiTick
      }
  in
    Midi.Recording
      { header: header
      , tracks: singleton track
      }

-- the following functions deal with interpreting repeated sections

-- | accumulate the MIDI messages from the List of bars
accumulateMessages :: List MidiBar -> List Midi.Message
accumulateMessages mbs =
  reverse $ concatMap _.midiMessages mbs

-- | select a subset of MIDI bars
barSelector :: Int -> Int -> MidiBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection :: MidiBars -> List Midi.Message -> Section -> List Midi.Message
repeatedSection mbs acc section =
  if (variantCount section > 1) then
    (variantSlices mbs section) <> acc
  else
    simpleRepeatedSection mbs acc section

-- | simple repeated sections with no variants
simpleRepeatedSection :: MidiBars -> List Midi.Message -> Section -> List Midi.Message
-- we could represent this with the next, but I think it's clearer having them separate
simpleRepeatedSection mbs acc (Section { start: Just a, end: Just d, repeatCount: 0 }) =
  (trackSlice a d mbs) <> acc
-- a repeated section
simpleRepeatedSection mbs acc (Section { start: Just a, end: Just d, repeatCount: n }) =
  let
    slice = trackSlice a d mbs
    slices = replicate (n + 1) slice
  in
    (concat slices) <> acc
-- something else (unexpected)
simpleRepeatedSection _ acc _ =
  acc

-- | build the notes from a subsection of the track
trackSlice :: Int -> Int -> List MidiBar -> List Midi.Message
trackSlice start finish mbs =
  accumulateMessages $ filter (barSelector start finish) mbs

-- build a variant slice for the variant denoted by index and pos
-- in the (active) variantEndings array
-- index is the current index into the array of varianty endings 
-- pos is the value at the current index
-- we need to use indexed methods because we need to look up the next index position
variantSlice :: Int -> Int -> Section -> MidiBars -> Tuple Int Int -> List Midi.Message
variantSlice start end section sectionBars (Tuple index pos) =
  let
    -- the first slice is the main tune section which is always from the 
    -- start to the first volta 
    firstEnding :: Int
    firstEnding = fromMaybe start $ variantPositionOf 0 section
    -- this is the current volta we're looking at
    thisEnding = pos
    -- this next bit is tricky
    --
    -- In the case of 
    --
    --     ..|1 ..:|2 ..:|3 ..:|4 ....
    --
    -- then each variant takes as its ending the start of the next variant
    -- except for the final one which must take the end of the enire section.
    --
    -- In the case of 
    --
    --     ..|1,3  :|2,4 ;|..
    --
    -- then this is true, except that also variant 2 must take its ending 
    -- as the end of the entire section.
    -- 
    -- We thus find a candidate ending for the volta (which may not exist).
    -- We'll use it for any variant other than the last, buut reject it in
    -- favour of end if the resulting bar position falls before the start
    -- position of the variant.

    -- find the end bar number position of the repeat at this index
    nextEnding = findEndingPosition (unwrap section).variantPositions index end
  {- 
  _ = spy "index" index
  _ = spy "variant count" (variantCount section)
  _ = spy "veryfirstEnding" firstEnding
  _ = spy "thisEnding" thisEnding
  _ = spy "nextEnding" nextEnding
  -}
  in
    trackSlice start firstEnding sectionBars
      <> trackSlice thisEnding nextEnding sectionBars

variantSlices :: MidiBars -> Section -> List Midi.Message
variantSlices mbs section =
  case section of
    Section { start: Just start, end: Just end } ->
      accumulateSlices mbs start end section
    _ ->
      Nil

-- accumulate all the slices for the variant endings
accumulateSlices :: MidiBars -> Int -> Int -> Section -> List Midi.Message
accumulateSlices mbs start end section =
  let
    sectionBars :: MidiBars
    sectionBars = filter (barSelector start end) mbs

    slices :: Array (List Midi.Message)
    slices = map
      (variantSlice start end section sectionBars)
      (activeVariants section)
  in
    concat $ Array.toUnfoldable slices

-- | Curtail the duration of the note by taking account of any grace notes
-- | that it may have
curtailedGracedNote :: Maybe Grace -> AbcNote -> AbcNote
curtailedGracedNote maybeGrace abcNote =
  case maybeGrace of
    Just grace ->
      let
        totalFraction = (fromInt $ Nel.length grace.notes) * graceFraction
        duration = abcNote.duration - (abcNote.duration * totalFraction)
      in
        abcNote { duration = duration }
    _ ->
      abcNote

-- | Calculate an individual grace note with its duration dependent on a
-- | fraction of the note that it graces
individualGraceNote :: AbcNote -> AbcNote -> AbcNote
individualGraceNote abcNote graceNote =
  graceNote { duration = graceFraction * abcNote.duration }

-- | build any repeated section into an extended melody with all repeats realised -}
buildRepeatedMelody :: List MidiBar -> Sections -> Midi.Track
buildRepeatedMelody mbs sections =
  -- trace "Sections" \_ ->
  -- traceShow sections \_ ->
  if (null sections) then
    Midi.Track Nil
  else
    Midi.Track $ foldl (repeatedSection mbs) Nil sections

-- | add a grace note (which may be defined outside the tuplet) to the first
-- | note inside the tuplet (assuming it is a note and not a rest)
-- | n.b. The external grace takes precedence over any internal grace
gracifyFirstNote :: Maybe Grace -> NonEmptyList RestOrNote -> List RestOrNote
gracifyFirstNote maybeGrace restsOrNotes =
  let
    hd = Nel.head restsOrNotes
    tl = Nel.tail restsOrNotes

    f :: RestOrNote -> RestOrNote
    f =
      bimap identity (\gn -> gn { maybeGrace = maybeGrace })
  in
    Cons (f hd) tl

-- temp debug
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
