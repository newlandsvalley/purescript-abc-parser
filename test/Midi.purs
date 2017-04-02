module Test.Midi (midiSuite) where

import Prelude (Unit, bind, show, (<>), (*))
import Control.Monad.Free (Free)
import Data.List (List(..), head, (:))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)

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
midiSuite =
  suite "midi" do
    test "notes" do
      assertMidi "| CDE |\r\n"
        (Midi.Track (standardTempo <> noteC 1 <> noteD 1 <> noteE 1))
    test "long notes" do
      assertMidi "| C2D2E2 |\r\n"
        (Midi.Track (standardTempo <> noteC 2 <> noteD 2 <> noteE 2))
    test "bars" do
      assertMidi "| C | D | E | F |\r\n"
        (Midi.Track (standardTempo <> noteC 1 <> noteD 1 <> noteE 1 <> noteF 1))
    test "lines" do
      assertMidi "| CD |\r\n| E |\r\n"
        (Midi.Track (standardTempo <> noteC 1 <> noteD 1 <> noteE 1))


-- these functions are helpers to build a MIDI target track
standardTempo :: List Midi.Message
standardTempo =
  Midi.Message 0 (Midi.Tempo 250000)
  : Nil

noteC :: Int -> List Midi.Message
noteC abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 60 80)
  : Midi.Message (standardMidiTick * abcDuration) (Midi.NoteOff 0 60 80)
  : Nil

noteD :: Int -> List Midi.Message
noteD abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 62 80)
  : Midi.Message (standardMidiTick * abcDuration) (Midi.NoteOff 0 62 80)
  : Nil

noteE :: Int -> List Midi.Message
noteE abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 64 80)
  : Midi.Message (standardMidiTick * abcDuration) (Midi.NoteOff 0 64 80)
  : Nil

noteF :: Int -> List Midi.Message
noteF abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 65 80)
  : Midi.Message (standardMidiTick * abcDuration) (Midi.NoteOff 0 65 80)
  : Nil

noteG :: Int -> List Midi.Message
noteG abcDuration =
  Midi.Message 0 (Midi.NoteOn 0 67 80)
  : Midi.Message (standardMidiTick * abcDuration) (Midi.NoteOff 0 67 80)
  : Nil
