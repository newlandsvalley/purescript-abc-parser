module Test.Tempo (tempoSpec) where

import Prelude (Unit, discard, ($))

import Data.Abc.Tempo (getBpm, setBpm, midiTempo, defaultAbcTempo, beatsPerSecond)
import Test.Utils
import Data.Rational (fromInt, (%))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


tempoSpec :: Spec Unit
tempoSpec = do
  describe "tempo" do
    it "gets the tempo from header" do
      assertIntFuncMatches
        fullHeaderHigh
        getBpm
        132
    it "gets the default tempo when there's no header" do
      assertIntFuncMatches
        noHeader
        getBpm
        120
    it "alters tempo of existing header" do
      assertMoveMatches
        fullHeaderMed
        (setBpm 132)
        fullHeaderHigh
    it "provides a new tempo from default of no headers" do
      assertMoveMatches
        noHeader
        (setBpm 144)
        justTempoHeader
    it "provides a new tempo from default of only Key header" do
      assertMoveMatches
        onlyKeyHeader
        (setBpm 84)
        justTempoAndKeyHeader
    -- | standard tempo is 1/4 = 120 (120 quarter notes/min)
    -- | i.e. 2 quarter notes/sec (each quarter note takes 1/2 sec)
    -- | but default ABC tempo uses eighth notes
    -- | so these last for 1/4 sec = 250000 μsec
    it "provides a MIDI tempo for default ABC tempo" do
      250000 `shouldEqual` (midiTempo defaultAbcTempo)
    it "provides a default bps" do
      (fromInt 2) `shouldEqual` (beatsPerSecond defaultAbcTempo)
    it "calculates a faster bps" do
      (fromInt 3)  `shouldEqual` (beatsPerSecond $ defaultAbcTempo { bpm = 180 })
    -- | bps should be unaffected by unit note length
    it "provides an accurate bps irrespective of a shorter notelen" do
      (fromInt 2) `shouldEqual` (beatsPerSecond $ defaultAbcTempo { unitNoteLength = 1 % 16 })

fullHeaderMed =
  "X: 1\x0D\nT: a title\x0D\nQ: 1/4=120\x0D\nM: 3/4\x0D\nK: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"

fullHeaderHigh =
  "X: 1\x0D\nT: a title\x0D\nQ: 1/4=132\x0D\nM: 3/4\x0D\nK: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"

noHeader =
  "| A,B, (3CDE [FG] |\x0D\n"

justTempoHeader =
  "Q: 1/4=144\x0D\n| A,B, (3CDE [FG] |\x0D\n"

onlyKeyHeader =
  "K: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"

justTempoAndKeyHeader =
  "Q: 1/4=84\x0D\nK: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"
