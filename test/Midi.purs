module Test.Midi (midiSuite) where

import Prelude (Unit, bind, show, (<>), (*), (<<<))
import Control.Monad.Free (Free)
import Data.List (List(..), head, (:))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Rational (Rational, fromInt, rational, toNumber)
import Data.Int (round)

import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi)
import Data.Midi as Midi
import Data.Abc.Tempo (standardMidiTick)

import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

assertMidi :: forall e. String -> Midi.Track -> Test e
assertMidi s midiTrack =
  case (parse s) of
    Right tune ->
      let
        midiRecording :: Midi.Recording
        midiRecording = toMidi tune
        track0 = fromMaybe (Midi.Track Nil) (head (unwrap midiRecording).tracks)
      in
        Assert.equal midiTrack track0

    Left err ->
      failure ("parse failed: " <> (show err))

midiSuite :: forall t. Free (TestF t) Unit
midiSuite = do
  transformationSuite

transformationSuite :: forall t. Free (TestF t) Unit
transformationSuite =
  suite "transformation" do
    test "notes" do
      assertMidi "| CDE |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    test "tied notes" do  -- currently implemented by a delay induced by a rest
      assertMidi "| CD-D |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> rest (fromInt 1)))
    test "tie across bars" do
      assertMidi "| CD- | D |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> rest (fromInt 1)))
    test "long notes" do
      assertMidi "| C2D2E2 |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 2) <> noteD (fromInt 2) <> noteE (fromInt 2)))
    test "rest" do
      assertMidi "| CDZ |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> rest (fromInt 1)))
    test "long rest" do
      assertMidi "| CDZ2 |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> rest (fromInt 2)))
    test "bars" do
      assertMidi "| C | D | E | F |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteF (fromInt 1)))
    test "lines" do
      assertMidi "| CD |\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    test "tuplet" do
      assertMidi "| (3CDE |\r\n"
        (Midi.Track (standardTempo <> noteC (rational 2 3) <> noteD (rational 2 3) <> noteE (rational 2 3)))
    test "broken rhythm >" do
      assertMidi "| C>D |\r\n"
        (Midi.Track (standardTempo <> noteC (rational 3 2) <> noteD (rational 1 2)))
    test "broken rhythm <" do
      assertMidi "| C<D |\r\n"
        (Midi.Track (standardTempo <> noteC (rational 1 2) <> noteD (rational 3 2)))
    test "broken rhythm >>" do
      assertMidi "| C>>D |\r\n"
        (Midi.Track (standardTempo <> noteC (rational 7 4) <> noteD (rational 1 4)))
    test "broken rhythm <<" do
      assertMidi "| C<<D |\r\n"
        (Midi.Track (standardTempo <> noteC (rational 1 4) <> noteD (rational 7 4)))
    test "chord" do
      assertMidi "| [CEG] |\r\n"
        (Midi.Track (standardTempo <> chordC (fromInt 1)))
    test "long chord" do
      assertMidi "| [CEG]2 |\r\n"
        (Midi.Track (standardTempo <> chordC (fromInt 2)))
    test "tempo header" do
      assertMidi "Q: 1/4=180\r\n| CDE |\r\n"
        (Midi.Track (tempo (rational 2 3) <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    test "unit note length header" do
      assertMidi "L: 1/16\r\n| CDE |\r\n"
        (Midi.Track (tempo (rational 1 2) <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    test "key signature header" do
      assertMidi "K: D\r\n| CDE |\r\n"
        (Midi.Track (standardTempo <> noteCs (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1)))
    test "accidental impact" do  -- an accidental influences the pitch of notes later in the bar
      assertMidi "| ^CDEC |\r\n"
        (Midi.Track (standardTempo <> noteCs (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteCs (fromInt 1) ))
    test "change tempo" do
      assertMidi "| CD |\r\nQ: 1/4=180\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (rational 2 3) <> noteE (fromInt 1)))
    test "change tempo inline " do
      assertMidi "| CD | [Q: 1/4=180] | E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (rational 2 3) <> noteE (fromInt 1)))
    test "change unit note length" do
      assertMidi "| CD |\r\nL: 1/16\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (rational 1 2) <> noteE (fromInt 1)))
    test "change unit note length inline" do
      assertMidi "| CD | [L: 1/16] | E |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> tempo (rational 1 2) <> noteE (fromInt 1)))
    test "change key" do
      assertMidi "| CDE |\r\nK: D\r\n| C |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteCs (fromInt 1) ))
    test "change key inline" do
      assertMidi "| CDE | [K: D] | C |\r\n"
        (Midi.Track (standardTempo <> noteC (fromInt 1) <> noteD (fromInt 1) <> noteE (fromInt 1) <> noteCs (fromInt 1) ))


-- | the number of MIDI ticks that equates to 1/4=120
standardTicks :: Int
standardTicks = 250000

-- these functions are helpers to build a MIDI target track
standardTempo :: List Midi.Message
standardTempo =
  Midi.Message 0 (Midi.Tempo standardTicks)
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
  : Midi.Message (midiTicks abcDuration) (Midi.NoteOff 0 0 80)
  : Nil


midiTicks :: Rational -> Int
midiTicks r =
  (round <<< toNumber)  (fromInt standardMidiTick * r)
