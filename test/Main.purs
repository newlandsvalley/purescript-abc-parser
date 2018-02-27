module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Abc (abcSuite)
import Test.Metadata (metadataSuite)
import Test.Octave (octaveSuite)
import Test.Tempo (tempoSuite)
import Test.Accidentals (accidentalsSuite)
import Test.KeySignature (keySignatureSuite)
import Test.Transposition (transpositionSuite)
import Test.Midi (midiSuite)

main :: forall t.
        Eff
          ( console :: CONSOLE
          , testOutput :: TESTOUTPUT
          , avar :: AVAR
          | t
          )
          Unit
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
