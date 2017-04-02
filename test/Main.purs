module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Abc (abcSuite)
import Test.Notation (notationSuite)
import Test.Octave (octaveSuite)
import Test.Tempo (tempoSuite)
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
    notationSuite
    octaveSuite
    tempoSuite
    transpositionSuite
    midiSuite
