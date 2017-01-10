module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Data.Either (Either(..))
-- import Data.Maybe (Maybe(..))

-- import Abc.ParseTree ( PitchClass(..), Accidental(..), Mode(..), KeySignature)
import Abc (parse, parseKeySignature)
import Abc.Canonical (fromTune)

import Test.Unit (Test, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)

assertRoundTrip :: forall e. String -> Test e
assertRoundTrip s =
  assertCanonical s s

assertCanonical :: forall e. String -> String -> Test e
assertCanonical s canonical =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Right tune ->
                Assert.equal canonical (fromTune tune)

            Left err ->
                failure ("parse failed: " <> (show err))

assertKeySigParses :: forall e. String -> Test e
assertKeySigParses s =
    let
        parseResult =
            parseKeySignature s
    in
        case parseResult of
            Right res ->
                success

            Left err ->
                failure ("parse failed: " <> (show err))


main :: forall t1.
        Eff
          ( console :: CONSOLE
          , testOutput :: TESTOUTPUT
          , avar :: AVAR
          | t1
          )
          Unit
main = runTest do
  suite "parser" do
    test "headers" do
      assertRoundTrip "T: the title\r\n"
    test "key signature" do
      assertKeySigParses "G"
