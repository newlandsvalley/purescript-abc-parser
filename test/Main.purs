module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (describe)
import Test.Abc (abcSpec)
import Test.Metadata (metadataSpec)
import Test.Octave (octaveSpec)
import Test.Optics (opticsSpec)
import Test.Tempo (tempoSpec)
import Test.Accidentals (accidentalsSpec)
import Test.KeySignature (keySignatureSpec)
import Test.Transposition (transpositionSpec)
import Test.Midi (midiSpec)
import Test.Voice (voiceSpec)
import Test.UnitNote (unitNoteSpec)
import Test.Normaliser (normaliserSpec)


main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter] do
  describe "ABC parser" do
    abcSpec
    accidentalsSpec
    keySignatureSpec
    metadataSpec
    midiSpec
    octaveSpec
    opticsSpec
    tempoSpec
    transpositionSpec
    unitNoteSpec
    voiceSpec
    normaliserSpec
    