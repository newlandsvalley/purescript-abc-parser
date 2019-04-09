module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Abc (abcSuite)
import Test.Metadata (metadataSuite)
import Test.Octave (octaveSuite)
import Test.Tempo (tempoSuite)
import Test.Accidentals (accidentalsSuite)
import Test.KeySignature (keySignatureSuite)
import Test.Transposition (transpositionSuite)
import Test.Midi (midiSuite)
import Test.Voice (voiceSuite)

main :: Effect  Unit
main = runTest do
  suite "parser" do
    abcSuite
    metadataSuite
    octaveSuite
    tempoSuite
    accidentalsSuite
    keySignatureSuite
    transpositionSuite
    midiSuite
    voiceSuite
