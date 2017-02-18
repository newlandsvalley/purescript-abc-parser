module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Free (Free)

import Data.Either (Either(..))
-- import Data.Maybe (Maybe(..))

-- import Abc.ParseTree ( PitchClass(..), Accidental(..), Mode(..), KeySignature)
import Abc (parse, parseKeySignature)
import Abc.Canonical (fromTune)

import Test.Unit (Test, TestF, suite, test, success, failure)
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
     -- headerSuite
     noteSuite


noteSuite :: forall t. Free (TestF t) Unit
noteSuite =
  suite "note" do
    test "single duration" do
       assertRoundTrip "| A |\r\n"
    test "doubly implied half duration" do
       assertRoundTrip "| B/ |\r\n"
    test "implied half duration" do
       assertCanonical "| B/2 |\r\n" halfNoteCanonical
    test "explicit half duration" do
       assertCanonical "| B1/2 |\r\n" halfNoteCanonical
    test "quarter duration" do
       assertCanonical "| D// |\r\n" quarterNoteCanonical
    test "double duration" do
       assertRoundTrip "| a2 |\r\n"
    test "broken rhythm >" do
       assertRoundTrip "| a>b |\r\n"
    test "broken rhythm <" do
       assertRoundTrip "| c<d |\r\n"
    test "triplet" do
       assertRoundTrip "| (3efg) |\r\n"



keySigSuite :: forall t. Free (TestF t) Unit
keySigSuite =
  suite "key signature parser" do
    test "key signature" do
      assertKeySigParses "G"

-- this fails at the moment
headerSuite :: forall t. Free (TestF t) Unit
headerSuite =
  suite "headers" do
    test "title" do
      assertRoundTrip "T: the title\r\n"

-- these ABC samples are already in canonical format which should allow round-tripping to work
-- because of the exact string matching algorithm

halfNoteCanonical =
    "| B/ |\r\n"

quarterNoteCanonical =
    "| D1/4 |\r\n"
