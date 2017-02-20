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

assertParses :: forall e. String -> Test e
assertParses s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Right res ->
                success

            Left err ->
                failure ("parse key signature failed: " <> (show err))

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
     headerSuite
     noteSuite
     barSuite


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
    test "double sharp" do
       assertRoundTrip "| ^^C2 |\r\n"
    test "sharp" do
       assertRoundTrip "| ^C/ |\r\n"
    test "double flat" do
       assertRoundTrip "| __C |\r\n"
    test "flat" do
       assertRoundTrip "| _C3/2 |\r\n"
    test "natural" do
       assertRoundTrip "| =C3/2 |\r\n"

barSuite :: forall t. Free (TestF t) Unit
barSuite =
  suite "bar lines" do
    test "repeat" do
      assertRoundTrip "|: A :|\r\n"
    test "bracket line" do
      assertRoundTrip "[| A |]\r\n"
    test "alternate endings" do
      assertRoundTrip "| A |1 B :|2 c||\r\n"
    test "double colon" do
      assertCanonical "||: A :: c :||\r\n" "||: A :|: c :||\r\n"


keySigSuite :: forall t. Free (TestF t) Unit
keySigSuite =
  suite "key signature parser" do
    test "key signature" do
      assertKeySigParses "G"


-- this fails at the moment
headerSuite :: forall t. Free (TestF t) Unit
headerSuite =
  suite "headers" do
    test "area" do
      assertRoundTrip "A: London\x0D\n| ABC |\x0D\n"
    test "book" do
      assertRoundTrip "B: Richie Robinson\x0D\n| ABC |\x0D\n"
    test "composer" do
      assertRoundTrip "C: Bys-Kalle\x0D\n| ABC |\x0D\n"
    test "discography" do
      assertRoundTrip "D: 2 Brudetstykke\x0D\n| ABC |\x0D\n"
    test "file URL" do
      assertRoundTrip "F: http\\\\tradtunedb.org.uk\x0D\n| ABC |\x0D\n"
    test "group" do
      assertRoundTrip "G: Swåp\x0D\n| ABC |\x0D\n"
    test "history" do
      assertRoundTrip "H: Learned from AnnbjØrg Lien\x0D\n| ABC |\x0D\n"
    test "instruction" do
      assertRoundTrip "I: abc-charset UTF-8\x0D\n| ABC |\x0D\n"
    test "key" do
      assertRoundTrip "K: Adorian\x0D\n| ABC |\x0D\n"
    test "key with accidental" do
      assertRoundTrip "K: Aminor ^f\x0D\n| ABC |\x0D\n"
    test "key with unspaced accidental" do
      assertCanonical "K: Eminor^c\x0D\n| ABC |\x0D\n" keyWithAccidental
    test "simple key" do
      assertCanonical "K: C\x0D\n| ABC |\x0D\n" keyCMajor
    test "key trailing space" do
      assertCanonical "K: Cmajor \x0D\n| ABC |\x0D\n" keyCMajor
    test "note length" do
        assertRoundTrip "L: 1/8\x0D\n| ABC |\x0D\n"
    test "meter" do
        assertRoundTrip "M: 3/4\x0D\n| ABC |\x0D\n"
    test "no meter" do
      assertRoundTrip "M: none\x0D\n| ABC |\x0D\n"
    test "macro" do
       assertRoundTrip "m: ~g2 = {a}g{f}g\x0D\n| ABC |\x0D\n"
    test "notes" do
       assertRoundTrip "N: from recording made at Tideswell\x0D\n| ABC |\x0D\n"
    test "origin" do
      assertRoundTrip "O: Skåne\x0D\n| ABC |\x0D\n"
    test "parts" do
      assertRoundTrip "P: ((AB)3.(CD)3)2\x0D\n| ABC |\x0D\n"
    test "tempo" do
      assertRoundTrip standardTempo
    test "suffixed tempo" do
      assertRoundTrip suffixedTempo
    test "prefixed tempo" do
      assertCanonical "Q: \"lento\" 1/4=70\x0D\n| ABC |\x0D\n" suffixedTempo
    test "degenerate tempo" do
      assertCanonical "Q: 120\x0D\n| ABC |\x0D\n" standardTempo
    test "tempo trailing space" do
      assertCanonical "Q: 1/4=120  \x0D\n| ABC |\x0D\n" standardTempo
    test "multi-beat tempo" do
      assertRoundTrip "Q: 1/4 3/8 1/4 3/8=40\x0D\n| ABC |\x0D\n"
    test "remark" do
      assertRoundTrip "r: this is a remark\x0D\n| ABC |\x0D\n"
    test "rhythm" do
      assertRoundTrip "R: Polska\x0D\n| ABC |\x0D\n"
    test "source" do
      assertRoundTrip "S: Christine Dyer\x0D\n| ABC |\x0D\n"
    test "title" do
      assertRoundTrip "T: Engelska efter Albert Augustsson\x0D\n| ABC |\x0D\n"
    test "user-defined" do
      assertRoundTrip "U: some comment\x0D\n| ABC |\x0D\n"
    test "voice" do
      assertRoundTrip "V: T1           clef=treble-8  name=\"Tenore I\"   snm=\"T.I\"\x0D\n| ABC |\x0D\n"
    test "words after" do
      assertRoundTrip "W: doh re mi fa \x0D\n| ABC |\x0D\n"
    -- the words aligned header only appears inline
    test "words aligned" do
      assertRoundTrip "| ABC |\x0D\nw: doh re mi fa \x0D\n| ABC |\x0D\n"
    test "reference" do
      assertRoundTrip "X: 125\x0D\n| ABC |\x0D\n"
    test "transcriber" do
      assertRoundTrip "Z: John Watson\x0D\n| ABC |\x0D\n"
    test "field continuation" do
      assertRoundTrip "R: Polska\x0D\n+: in triplet time\x0D\n| ABC |\x0D\n"
    test "comment" do
      assertRoundTrip "%%TBL:{\"version\":\"beta\",\"type\":\"tune\",\"id\":\"10294\"}\x0D\n| ABC |\x0D\n"
    test "unsupported header" do
      assertParses "j: custom header\x0D\n| ABC |\x0D\n"
    test "bracket in header" do
      assertRoundTrip "r: this is a remark [part 1]\x0D\n| ABC |\x0D\n"


-- these ABC samples are already in canonical format which should allow round-tripping to work
-- because of the exact string matching algorithm

keyWithAccidental =
    "K: Eminor ^c\x0D\n| ABC |\x0D\n"

keyCMajor =
    "K: Cmajor\x0D\n| ABC |\x0D\n"

halfNoteCanonical =
    "| B/ |\r\n"

quarterNoteCanonical =
    "| D1/4 |\r\n"

standardTempo =
    "Q: 1/4=120\x0D\n| ABC |\x0D\n"

suffixedTempo =
    "Q: 1/4=70 \"lento\"\x0D\n| ABC |\x0D\n"
