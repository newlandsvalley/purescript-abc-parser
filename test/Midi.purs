module Test.Midi (midiSpec) where

import Effect.Aff (Aff)
import Data.Abc (AbcNote, Accidental(..), Mode(..), ModifiedKeySignature, PitchClass(..)) 
import Data.Abc.Midi (MidiPitch, toMidiRecording, toMidiRecordingAtBpm, toMidiPitch)
import Data.Abc.Parser (parse)
import Data.Abc.Repeats.Types (VariantPositions)
import Data.Abc.Repeats.Variant (findEndingPosition)
import Data.Abc.Tempo (standardMidiTick)
import Data.Either (Either(..))
import Data.Int (round)
import Data.List (List(..), head, (:))
import Data.Map (empty, fromFoldable)
import Data.Maybe (fromMaybe)
import Data.Midi as Midi
import Data.Rational (Rational, fromInt, toNumber, (%))
import Data.Tuple (Tuple(..))
import Prelude (Unit, discard, show, (<>), (*), (<<<))
import Test.Utils (buildKeySig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


assertMidi :: String -> Midi.Track -> Aff Unit
assertMidi s midiTrack =
  case (parse s) of
    Right tune ->
      let
        Midi.Recording midiRecording = toMidiRecording tune
        track0 = fromMaybe (Midi.Track Nil) (head midiRecording.tracks)
      in
        midiTrack `shouldEqual` track0

    Left err ->
      fail ("parse failed: " <> (show err))

assertMidiAtBpm :: String -> Int -> Midi.Track -> Aff Unit
assertMidiAtBpm s bpm midiTrack =
  case (parse s) of
    Right tune ->
      let
        Midi.Recording midiRecording = toMidiRecordingAtBpm tune bpm
        track0 = fromMaybe (Midi.Track Nil) (head midiRecording.tracks)
      in
        midiTrack  `shouldEqual` track0

    Left err ->
      fail ("parse failed: " <> (show err))

-- | assert the MIDI Pitch of an ABCNote in the context of G Major 
-- | but with no accidentals occurring in the nominal bar where the note lives
assertMidiPitch :: AbcNote -> MidiPitch -> Aff Unit
assertMidiPitch abcNote target = 
  pitch `shouldEqual` target

  where 
    pitch = 
      toMidiPitch gMajor empty abcNote

midiSpec :: Spec Unit
midiSpec = do
  transformationSpec
  repeatSpec
  variantSpec
  graceSpec
  atTempoSpec
  pitchSpec

transformationSpec :: Spec Unit
transformationSpec =
  describe "MIDI transformation" do
    it "handles notes" do
      assertMidi "| CDE |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles tied notes" do
      assertMidi "| CD-D |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 2)))
    it "handles doubly tied notes" do
      assertMidi "| CD-D-D |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 3)))
    it "handles a tie across bars" do
      assertMidi "| CD- | D |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 2)))
    it "handles long notes" do
      assertMidi "| C2D2E2 |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 2) <> noteD (fromInt 2) <> noteE (fromInt 2)))
    it "handles a rest" do
      assertMidi "| CDZ |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> rest (fromInt 1)))
    it "handles a long rest" do
      assertMidi "| CDZ2 |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> rest (fromInt 2)))
    it "handles bars" do
      assertMidi "| C | D | E | F |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteF (fromInt 1)))
    it "handles lines" do
      assertMidi "| CD |\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles tuplets" do
      assertMidi "| (3CDE |\r\n"
        (Midi.Track (standardTempo <> noteC (2 % 3) <> noteD (2 % 3) <> noteE (2 % 3)))
    it "handles a tuplet with rest" do
      assertMidi "| (3zDE |\r\n"
        (Midi.Track (standardTempo <> rest (2 % 3) <> noteD (2 % 3) <> noteE (2 % 3)))
    it "handles broken rhythm >" do
      assertMidi "| C>D |\r\n"
        (Midi.Track (standardTempo <> noteC (3 % 2) <> noteD (1 % 2)))
    it "handles broken rhythm <" do
      assertMidi "| C<D |\r\n"
        (Midi.Track (standardTempo <> noteC (1 % 2) <> noteD (3 % 2)))
    it "handles broken rhythm >>" do
      assertMidi "| C>>D |\r\n"
        (Midi.Track (standardTempo <> noteC (7 % 4) <> noteD (1 % 4)))
    it "handles broken rhythm <<" do
      assertMidi "| C<<D |\r\n"
        (Midi.Track (standardTempo <> noteC (1 % 4) <> noteD (7 % 4)))
    it "handles chords" do
      assertMidi "| [CEG] |\r\n"
        (Midi.Track (standardTempo <> chordC (fromInt 1)))
    it "handles along chord" do
      assertMidi "| [CEG]2 |\r\n"
        (Midi.Track (standardTempo <> chordC (fromInt 2)))
    it "handles an equivalent long chord" do
      assertMidi "| [C2E2G2] |\r\n"
        (Midi.Track (standardTempo <> chordC (fromInt 2)))
    it "handles a doubly fractional chord" do
      assertMidi "| [C/E/G/]1/3 |\r\n"
        (Midi.Track (standardTempo <> chordC (1 % 6)))
    it "handles a tie into chord" do -- we don't support ties into chords - it's ambiguous
      assertMidi "| C-[CEG] |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> chordC (fromInt 1)))
    it "respects a tempo header" do
      assertMidi "Q: 1/4=180\r\n| CDE |\r\n"
        (Midi.Track (tempo (2 % 3) <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "respects a unit note length header" do
      assertMidi "L: 1/16\r\n| CDE |\r\n"
        (Midi.Track (tempo (1 % 2) <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "respects a key signature header" do
      assertMidi "K: D\r\n| CDE |\r\n"
        (Midi.Track (standardTempo <> noteCs (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "acknowledges accidental impact" do -- an accidental influences the pitch of notes later in the bar
      assertMidi "| ^CDEC |\r\n"
        (Midi.Track (standardTempo <> noteCs (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteCs (fromInt 1)))
    it "hadles a change of tempo" do
      assertMidi "| CD |\r\nQ: 1/4=180\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (2 % 3) <> noteE (fromInt 1)))
    it "handles a change of tempo inline " do
      assertMidi "| CD | [Q: 1/4=180] | E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (2 % 3) <> noteE (fromInt 1)))
    it "respects a change unit note length" do
      assertMidi "| CD |\r\nL: 1/16\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (1 % 2) <> noteE (fromInt 1)))
    it "respects a change unit note length inline" do
      assertMidi "| CD | [L: 1/16] | E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (1 % 2) <> noteE (fromInt 1)))
    it "respects a key change" do
      assertMidi "| CDE |\r\nK: D\r\n| C |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteCs (fromInt 1)))
    it "respects a key change inline" do
      assertMidi "| CDE | [K: D] | C |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteCs (fromInt 1)))

repeatSpec :: Spec Unit
repeatSpec =
  describe "repeats" do
    it "handles a simple repeat" do
      assertMidi "|: CDE :|\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
            )
        )
    it "handles a lead-in then repeat" do
      assertMidi "FC |: CDE :|\r\n"
        ( Midi.Track
            ( standardTempo <> noteF (fromInt 1) <> noteC (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
            )
        )
    it "handles a pair of repeats" do
      assertMidi "|: CDE :|: DEF :|\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteF (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteF (fromInt 1)
            )
        )
    it "handles a simple repeat implicit start" do
      assertMidi "| CDE :|\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> standardTempo
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
            )
        )
    it "handles a simple repeat then unrepeated" do
      assertMidi "|: CDE :| F |\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteF (fromInt 1)
            )
        )
    it "handles unrepeated then simple repeat" do
      assertMidi "| F |: CDE :|\r\n"
        ( Midi.Track
            ( standardTempo <> noteF (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
            )
        )
    it "handles alternate endings" do
      assertMidi "|: CD |1 E :|2 F |\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteF (fromInt 1)
            )
        )
    it "handles alternate endings then repeat" do
      assertMidi "|: CD |1 E :|2 F |: CDE :|\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteF (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
            )
        )
    it "handles an alternate endings list" do
      assertMidi "|: CD |1,3 E :|2 F |\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteF (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
            )
        )
    it "handles an alternate endings range" do
      assertMidi "|: CD |1-3 E :|4 F |\r\n"
        ( Midi.Track
            ( standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteE (fromInt 1)
                <> noteC (fromInt 1)
                <> noteD (fromInt 1)
                <> noteF (fromInt 1)
            )
        )

-- different types of variants (voltas) in repeated sections
variantSpec :: Spec Unit
variantSpec =
  describe "next position" do
    it "handles sample1 at 1,2,3 and 4" do
      8 `shouldEqual` (findEndingPosition variant1 1 end)
      8 `shouldEqual` (findEndingPosition variant1 2 end)
      8 `shouldEqual` (findEndingPosition variant1 3 end)
      end `shouldEqual` (findEndingPosition variant1 4 end)
    it "handles sample2 at 1,2,3 and 4" do
      8 `shouldEqual` (findEndingPosition variant2 1 end)
      end `shouldEqual` (findEndingPosition variant2 2 end)
      8 `shouldEqual` (findEndingPosition variant2 3 end)
      end `shouldEqual` (findEndingPosition variant2 4 end)
    it "handle sample3 at 1,2,3 and 4" do
      4 `shouldEqual` (findEndingPosition variant3 1 end)
      6 `shouldEqual` (findEndingPosition variant3 2 end)
      8 `shouldEqual` (findEndingPosition variant3 3 end)
      end `shouldEqual` (findEndingPosition variant3 4 end)

-- each grace note 'steals' 10% of the note it graces
graceSpec :: Spec Unit
graceSpec =
  describe "grace notes" do
    it "handles single grace" do
      assertMidi "| {D}CDE |\r\n"
        (Midi.Track (standardTempo <> noteD (1 % 10) <> noteC (9 % 10) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles double grace" do
      assertMidi "| {ED}CDE |\r\n"
        (Midi.Track (standardTempo <> noteE (1 % 10) <> noteD (1 % 10) <> noteC (8 % 10) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles graces immediately after ties are ignored" do
      assertMidi "| C-{D}CDE |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 2) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles graces before ties are accumulated" do
      assertMidi "| {D}C-CDE |\r\n"
        (Midi.Track (standardTempo <> noteD (1 % 10) <> noteC (19 % 10) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles graces inside tuplets" do
      assertMidi "| (3C{E}DE |\r\n"
        (Midi.Track (standardTempo <> noteC (2 % 3) <> noteE (2 % 30) <> noteD (18 % 30) <> noteE (2 % 3)))
    it "handles graces immediately preceding tuplets" do
      assertMidi "| {E}(3CDE |\r\n"
        (Midi.Track (standardTempo <> noteE (2 % 30) <> noteC (18 % 30) <> noteD (2 % 3) <> noteE (2 % 3)))
    
      assertMidi "| C>{E}D |\r\n"
        (Midi.Track (standardTempo <> noteC (3 % 2) <> noteE (1 % 20) <> noteD (9 % 20)))

atTempoSpec :: Spec Unit
atTempoSpec =
  describe "set tempo externally" do
    it "handles an identical tempo" do
      assertMidiAtBpm "| CDE |\r\n" 120
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    it "handles half tempo" do
      assertMidiAtBpm "| CDE |\r\n" 60
        (Midi.Track (halfTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))

pitchSpec :: Spec Unit
pitchSpec =
  describe "convert a note's pitch class to a MIDI pitch (in context)" do  
    it "converts B natural to MIDI" do         
      assertMidiPitch b 47 
    it "converts B# to MIDI" do         
      assertMidiPitch bSharp 48
    it "converts B## to MIDI" do         
      assertMidiPitch bDoubleSharp 49
    it "converts C natural to MIDI" do         
      assertMidiPitch c 48

-- | the number of MIDI ticks that equates to 1/4=120
standardTicks :: Int
standardTicks = 250000

-- these functions are helpers to build a MIDI target track
standardTempo :: List Midi.Message
standardTempo =
  Midi.Message 0 (Midi.Tempo standardTicks)
    : Nil

-- half the standard tempo - a MIDI tick is twice as long
halfTempo :: List Midi.Message
halfTempo =
  Midi.Message 0 (Midi.Tempo (standardTicks * 2))
    : Nil

tempo :: Rational -> List Midi.Message
tempo r =
  let
    tmpo = (round <<< toNumber) (r * fromInt standardTicks)
  in
    Midi.Message 0 (Midi.Tempo tmpo)
      : Nil

rest :: Rational -> List Midi.Message
rest abcDuration =
  Midi.Message (midiTicks abcDuration) (Midi.NoteOn 0 0 80)
    : Nil

noteC :: Rational -> List Midi.Message
noteC abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 60 80)
    : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 60 80)
    : Nil

noteCs :: Rational -> List Midi.Message
noteCs abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 61 80)
    : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 61 80)
    : Nil

noteD :: Rational -> List Midi.Message
noteD abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 62 80)
    : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 62 80)
    : Nil

noteE :: Rational -> List Midi.Message
noteE abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 64 80)
    : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 64 80)
    : Nil

noteF :: Rational -> List Midi.Message
noteF abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 65 80)
    : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 65 80)
    : Nil

chordC :: Rational -> List Midi.Message
chordC abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 60 80)
    : Midi.Message 0 (Midi.NoteOn 0 64 80)
    : Midi.Message 0 (Midi.NoteOn 0 67 80)
    : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 60 80)
    : Midi.Message 0 (Midi.NoteOff 0 64 80)
    : Midi.Message 0 (Midi.NoteOff 0 67 80)
    : Nil

b :: AbcNote
b =
  { pitchClass: B, accidental: Implicit, octave: 3, duration: fromInt 1, tied: false }

bSharp :: AbcNote
bSharp =
  { pitchClass: B, accidental: Sharp, octave: 3, duration: fromInt 1, tied: false }

bDoubleSharp :: AbcNote
bDoubleSharp =
  { pitchClass: B, accidental: DoubleSharp, octave: 3, duration: fromInt 1, tied: false }

c :: AbcNote
c =
  { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

gMajor :: ModifiedKeySignature
gMajor =
  buildKeySig G Natural Major

--  |1,2,3 ...:|4.....|
variant1 :: VariantPositions
variant1 =
  fromFoldable [ Tuple 1 5, Tuple 2 5, Tuple 3 5, Tuple 4 8 ]

-- |1,3....:|2,4.....|
variant2 :: VariantPositions
variant2 =
  fromFoldable [ Tuple 1 5, Tuple 3 5, Tuple 2 8, Tuple 4 8 ]

-- |1....:|2....:|3....:|4.....|
variant3 :: VariantPositions
variant3 =
  fromFoldable [ Tuple 1 2, Tuple 2 4, Tuple 3 6, Tuple 4 8 ]

-- the end of a section with variants
end :: Int
end = 10

midiTicks :: Rational -> Int
midiTicks r =
  (round <<< toNumber) (fromInt standardMidiTick * r)
