-- | Conversion of an ABC tune to MIDI.
module Data.Abc.Midi
  ( MidiPitch
  , toMidi
  , toMidiAtBpm
  , toMidiPitch
  , midiPitchOffset) where

import Control.Monad.State (State, get, put, evalState)
import Data.Abc (AbcTune, AbcNote, Bar, RestOrNote, Pitch(..), Accidental(..), BarLine, 
    Broken(..), Header(..), TuneBody, BodyPart(..), Grace, GraceableNote, 
    MusicLine, Music(..), Mode(..), ModifiedKeySignature, 
    TempoSignature, PitchClass(..))
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Canonical as Canonical
import Data.Abc.KeySignature (modifiedKeySet, pitchNumber, notesInChromaticScale)
import Data.Abc.Metadata (dotFactor, getKeySig)
import Data.Abc.Midi.Types (MidiBar, MidiBars)
import Data.Abc.Midi.RepeatSections (initialRepeatState, indexBar, finalBar)
import Data.Abc.Repeats.Types (RepeatState, Section(..), Sections)
import Data.Abc.Repeats.Variant (activeVariants, findEndingPosition, variantPositionOf, variantCount)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, midiTempo, noteTicks, setBpm, standardMidiTick)
import Data.Either (Either(..))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl, oneOf)
import Data.Unfoldable (replicate)
import Data.Array as Array
import Data.List (List(..), (:), null, concat, concatMap, foldr, filter, reverse, singleton)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, length, tail, toList) as Nel
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi as Midi
import Data.Newtype (unwrap)
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (bind, identity, map, pure, ($), (&&), (*), (+), (-), (<), (>), (<>), (>=))

-- | The pitch of a note expressed as a MIDI interval.
type MidiPitch =
    Int

-- | the fraction  of the duration of a note that is 'stolen' by any
-- | preceding grace note that it has
graceFraction :: Rational
graceFraction =
  (1 % 10)

-- | Convert an ABC note pitch to a MIDI pitch.
-- |
-- | AbcNote - the note in question
-- | ModifiedKeySignature - the key signature (possibly modified by extra accidentals)
-- | Accidentals - any notes in this bar which have previously been set explicitly to an accidental which are thus inherited by this note
-- | MidiPitch - the resulting pitch of the MIDI note
toMidiPitch :: AbcNote -> ModifiedKeySignature -> Accidentals.Accidentals -> MidiPitch
toMidiPitch n mks barAccidentals =
  (n.octave * notesInChromaticScale) + midiPitchOffset n mks barAccidentals

-- | convert an AbcNote (pich class and accidental) to a pitch offset in a chromatic scale
midiPitchOffset :: AbcNote -> ModifiedKeySignature -> Accidentals.Accidentals -> Int
midiPitchOffset n mks barAccidentals =
  let
    inBarAccidental =
      Accidentals.lookup n.pitchClass barAccidentals

    inKeyAccidental =
      -- accidentalImplicitInKey n.pitchClass mks
      Accidentals.implicitInKeySet n.pitchClass (modifiedKeySet mks)

    -- look first for an explicit note accidental, then for an explicit for the same note that occurred earlier in the bar and
    -- finally look for an implicit accidental attached to this key signature
    accidental =
      case n.accidental of
        Implicit ->
          fromMaybe Natural $ oneOf ( inBarAccidental: inKeyAccidental: Nil )
        _ ->  -- explict
          n.accidental

    -- the lookup pattern just uses sharps or flats (if there) or the empty String if not
    accidentalPattern =
      Canonical.keySignatureAccidental accidental

    pattern =
      Pitch { pitchClass : n.pitchClass, accidental : accidental }
  in
    pitchNumber pattern

-- | Transform ABC into a MIDI recording.
toMidi :: AbcTune -> Midi.Recording
toMidi tune =
  do
    evalState (transformTune tune) (initialState tune)

-- | Transform ABC into a MIDI recording but at the mdified tempo
-- | defined by the new BPM  (beats per minute)
toMidiAtBpm :: AbcTune -> Int -> Midi.Recording
toMidiAtBpm originalTune bpm =
  let
    tune = setBpm bpm originalTune
  in
    evalState (transformTune tune) (initialState tune)

-- | the state to thread through the computation
type TState =
    { modifiedKeySignature ::  ModifiedKeySignature    -- the current key signature
    , abcTempo ::  AbcTempo                            -- the current tempo
    , currentBar :: MidiBar                            -- the current bar being translated
    , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
                                                       -- any notes marked explicitly as accidentals in the current bar
    , lastNoteTied :: Maybe AbcNote                    -- the last note, if it was tied?
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
  , endRepeats : 0
  , startRepeats : 0
  , iteration : Nothing
  , midiMessages : initialMsg : Nil
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarLine -> MidiBar
buildNewBar i barLine =
  {  number : i
  ,  endRepeats : barLine.endRepeats
  ,  startRepeats : barLine.startRepeats
  ,  iteration : barLine.iteration
  ,  midiMessages : Nil
  }

-- | default to C Major (i.e. no accidental modifiers)
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }, modifications: Nil }

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
          , lastNoteTied : Nothing
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
    _ <- transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TransformationState Midi.Recording
transformBodyPart bodyPart =
  case bodyPart of
    Score bars ->
      transformBarList bars
    BodyInfo header ->
      transformHeader header

transformBarList :: List Bar -> State TransformationState Midi.Recording
transformBarList Nil =
  do
    tpl <- get
    pure $ snd tpl
transformBarList (b : bs) =
  do
    _ <- transformBar b
    transformBarList bs

transformBar :: Bar -> State TransformationState Midi.Recording
transformBar bar =
  do
    -- save the bar to state
    _ <- updateState addBarToState bar.startLine
    transformMusicLine bar.music

transformMusicLine :: MusicLine -> State TransformationState Midi.Recording
transformMusicLine Nil =
  do
    tpl <- get
    pure $ snd tpl
transformMusicLine (l : ls) =
  do
    _ <- transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TransformationState Midi.Recording
transformMusic m =
  case m of
    Note graceableNote ->
      updateState (addGraceableNoteToState false (1 % 1)) graceableNote

    Rest r ->
      updateState addRestToState r.duration

    Tuplet t ->
      updateState (addTupletContentsToState t.maybeGrace (t.signature.q % t.signature.p)) t.restsOrNotes

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
          _ <- updateState (addNotesToState true (1 % 1)) (Nel.toList abcChord.notes)
          -- pace by adding a NoteOff for the overall duration
          _ <- updateState (addNoteOffToState duration) first
          -- and terminate all the other notes with a NoteOff at 0 duration
          updateState (addNoteOffsToState (fromInt 0)) others

    BrokenRhythmPair note1 broken note2 ->
      case broken of
        LeftArrow i ->
          do
            _ <- updateState (addGraceableNoteToState false (brokenTempo i false)) note1
            updateState (addGraceableNoteToState false (brokenTempo i true)) note2
        RightArrow i ->
          do
            _ <- updateState (addGraceableNoteToState false (brokenTempo i true)) note1
            updateState (addGraceableNoteToState false (brokenTempo i false)) note2

    Inline header ->
      transformHeader header

    _ ->
      do
        tpl <- get
        pure $ snd tpl

-- | add a bar to the state.  index it and add it to the growing list of bars
addBarToState :: TState -> BarLine -> TState
addBarToState tstate barLine =
  -- the current bar held in state is empty so we coalesce
  if (isBarEmpty tstate.currentBar) then
    coalesceBar tstate barLine
  -- it's not emmpty so we initialise the new bar
  else
    let
      currentBar = tstate.currentBar
      repeatState =
        indexBar currentBar tstate.repeatState
      -- ad this bar to the growing list of bars
      rawTrack =
        -- the current bar is not empty so we aggregate the new bar into the track
        currentBar : tstate.rawTrack
    in
      tstate { currentBar = buildNewBar (currentBar.number + 1) barLine
             , currentBarAccidentals = Accidentals.empty
             , repeatState = repeatState
             , rawTrack = rawTrack
             }

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: TState -> BarLine-> TState
coalesceBar tstate barLine =
  let
    endRepeats = tstate.currentBar.endRepeats + barLine.endRepeats
    startRepeats = tstate.currentBar.startRepeats + barLine.startRepeats
    bar' = tstate.currentBar { endRepeats = endRepeats
                             , startRepeats = startRepeats
                             , iteration = barLine.iteration 
                             }
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
    Key mks _ ->
      updateState addKeySigToState mks
    Tempo t ->
      updateState addTempoToState t
    _ ->
      do
        tpl <- get
        pure $ snd tpl

addGraceableNoteToState :: Boolean -> Rational -> TState-> GraceableNote -> TState
addGraceableNoteToState chordal tempoModifier tstate graceableNote =
  addNoteToState chordal tempoModifier graceableNote.maybeGrace tstate graceableNote.abcNote

-- | a note is added to the current barAccidentals as a NoteOn NoteOff pair
-- | possible preceded by its grace notes (if they exist)
-- | there are other implications for state - if the note has an explicit
-- | accidental, overriding the key then it is added to state because it
-- | influences other notes later in the bar
addNoteToState :: Boolean -> Rational -> Maybe Grace -> TState-> AbcNote -> TState
addNoteToState chordal tempoModifier maybeGrace tstate abcNote =
  let
    Tuple msgs newTie =
      if chordal then
        processChordalNote tempoModifier tstate abcNote
      else
        processNoteWithTie tempoModifier maybeGrace tstate abcNote
    barAccidentals =
      addNoteToBarAccidentals abcNote tstate.currentBarAccidentals
  in
    tstate { currentBar = tstate.currentBar { midiMessages = msgs }
           , lastNoteTied = newTie
           , currentBarAccidentals = barAccidentals
           }

-- | tuplets can now contain rests
addRestOrNoteToState :: Boolean -> Rational -> TState-> RestOrNote -> TState
addRestOrNoteToState chordal tempoModifier tstate restOrNote =
  case restOrNote of
    Left r ->
      --  modifiy the rest duration by the tempo modifier
      addRestToState tstate (r.duration * tempoModifier)
    Right n ->
      addGraceableNoteToState chordal tempoModifier tstate n

-- | process the incoming  note that is part of a chord.
-- | Here, ties and grace notes preceding the note
-- | are not supported
processChordalNote ::  Rational -> TState -> AbcNote -> Tuple (List Midi.Message) (Maybe AbcNote)
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
      toMidiPitch abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
    ticks =
      noteTicks (abcNote.duration * tempoModifier)
    in
      ((midiNoteOff ticks pitch) : (midiNoteOn 0 pitch) : Nil )

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
      toMidiPitch abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
  in
    midiNoteOn 0 pitch


-- | chordal means that the notes form a chord and thus do not need
-- | NoteOff messages to be generated after each note
addNotesToState :: Boolean -> Rational -> TState-> List AbcNote -> TState
addNotesToState chordal tempoModifier tstate abcNotes =
  foldl (addNoteToState chordal tempoModifier Nothing) tstate abcNotes

-- | Add the contents of a tuplet to state.  This is an optional grace note
-- | plus a list of either rests or notes.
-- | tempo modifier is the modification of the tempo indicate donly by the
-- | tuplet signature (e.g. 3 notes in the time of two)
addTupletContentsToState :: Maybe Grace -> Rational -> TState-> NonEmptyList RestOrNote -> TState
addTupletContentsToState mGrace tempoModifier tstate restsOrNotes =
  let
    -- move the grace notes from external to internal
    gracedRestsOrNotes = gracifyFirstNote mGrace restsOrNotes
  in
    foldl (addRestOrNoteToState false tempoModifier) tstate gracedRestsOrNotes

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

-- | work out the broken rhythm tempo
brokenTempo :: Int -> Boolean -> Rational
brokenTempo i isUp =
  if isUp then
    (fromInt 1) + (dotFactor i)
  else
    (fromInt 1) - (dotFactor i)

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
      Midi.Recording recording = snd tpl
      tstate = fst tpl
      currentBar = tstate.currentBar
      -- index the final bar and finalise the repear state
      repeatState =
        finalBar currentBar tstate.repeatState
      -- ensure we incorporate the very last bar
      tstate' = tstate { rawTrack = tstate.currentBar : tstate.rawTrack
                       , repeatState = repeatState }
      track = buildRepeatedMelody tstate'.rawTrack tstate'.repeatState.sections
      recording' :: Midi.Recording
      recording' = Midi.Recording recording { tracks = singleton $ track }
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


-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection ::  MidiBars -> List Midi.Message -> Section -> List Midi.Message 
repeatedSection mbs acc section = 
  if (variantCount section > 1) then 
    (variantSlices mbs section) <> acc
  else 
    simpleRepeatedSection mbs acc section    

-- | simple repeated sections with no variants
simpleRepeatedSection ::  MidiBars -> List Midi.Message -> Section -> List Midi.Message
-- we could represent this with the next, but I think it's clearer having them separate
simpleRepeatedSection mbs acc (Section { start: Just a, end: Just d, repeatCount : 0 }) =
  (trackSlice a d mbs ) <> acc
-- a repeated section
simpleRepeatedSection mbs acc (Section { start: Just a,  end: Just d, repeatCount : n }) =
  let 
    slice = trackSlice a d mbs 
    slices = replicate (n+1) slice
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
accumulateSlices :: MidiBars -> Int -> Int -> Section ->  List Midi.Message
accumulateSlices mbs start end section  = 
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
