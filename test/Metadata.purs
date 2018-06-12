module Test.Metadata (metadataSuite) where

import Prelude (Unit, discard, show, ($), (<>), (==))
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), intersect, length, (:))
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..))
import Data.Abc.Parser (parse)
import Data.Abc (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Pitch(..), KeySet, Mode(..), AbcNote, AbcTune)
import Data.Abc.Metadata
import Data.Abc.Accidentals as Accidentals

import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert

assertOkTitle :: String -> String -> Test
assertOkTitle source target =
  case parse source of
    Right tune ->
      let
        mtitle =
          getTitle tune
      in
        case mtitle of
          Just title ->
            Assert.equal target title

          _ ->
            failure "no title"

    _ ->
      failure "parse error"

assertOkKeySig :: String -> ModifiedKeySignature -> Test
assertOkKeySig source target =
  case parse source of
    Right tune ->
      let
        mkeySig =
          getKeySig tune
      in
        case mkeySig of
          Just keySig ->
            Assert.equal target.keySignature.pitchClass keySig.keySignature.pitchClass

          _ ->
            failure "no key signature"
    _ ->
      failure "parse error"

assertOkMeter :: String -> (Tuple Int Int) -> Test
assertOkMeter source target =
  case parse source of
    Right tune ->
      let
        meter =
          getMeter tune
      in
        case meter of
          Just m  ->
            Assert.equal target m

          _ ->
            failure "no meter"
    _ ->
      failure "parse error"

assertOkNoteLen :: String -> Rational -> Test
assertOkNoteLen source target =
  case parse source of
    Right tune ->
      let
        len =
          getUnitNoteLength tune
      in
        case len of
          Just rat  ->
            Assert.equal target rat

          _ ->
            failure "no unit note length"
    _ ->
      failure "parse error"

assertNoHeader :: forall h. String -> (AbcTune -> Maybe h) -> Test
assertNoHeader source getf =
  case parse source of
    Right tune ->
      let
        mtitle =
          getf tune
      in
        case mtitle of
          Just title ->
            failure "no title expected"
          _ ->
            success

    _ ->
      failure "parse error"

assertHeaderCount :: Int -> String ->  Test
assertHeaderCount expectedCount source =
  case parse source of
    Right tune ->
      Assert.equal expectedCount (length tune.headers)

    _ ->
      failure "parse error"

assertEquivalentKeys :: KeySet -> KeySet -> Test
assertEquivalentKeys actual expected =
  let
    intersection = intersect actual expected
  in
    {- debug
    if null expected then
      failure $ "debug: " <> (show actual)
    -}
    if (length intersection == length expected) then
      success
    else
      failure $ "non-equivalent keys: "
         <> (show actual) <> " not equal to: " <> (show expected)

{- It's such a pain to provide Eq, Show on what you'd like to be a somple record
   so for testing purposes just collapse tp a string

   Type class instances for type synonyms are disallowed
-}
showKeySig :: KeySignature -> String
showKeySig ks =
  show ks.pitchClass <> show ks.accidental <> show ks.mode

metadataSuite :: Free TestF Unit
metadataSuite = do
   headerSuite

headerSuite :: Free TestF Unit
headerSuite =
  suite "headers" do
   test "getTitle" do
     assertOkTitle titledTune "Gamal Reinlender"
   test "no title" do
     assertNoHeader keyedTune getTitle
   test "doubly titled tune" do
     assertOkTitle doublyTitledTune "Nancy Dawson"
   test "OK key header" do
     assertOkKeySig keyedTune fMajorM
   test "no key header" do
     assertNoHeader titledTune getKeySig
   test "multiple headers" do
     assertHeaderCount 8 manyHeaders
   test "getMeter" do
    assertOkMeter manyHeaders (Tuple 4 4)
   test "getUnitNoteLen" do
     assertOkNoteLen manyHeaders (1 % 16)


-- headers in sample ABC tunes
keyedTune =
    "K: FMajor\x0D\n| ABC |\x0D\n"

titledTune =
    "T: Gamal Reinlender\x0D\n| ABC |\x0D\n"

doublyTitledTune =
    "T: Nancy Dawson\x0D\nT: Piss Upon the Grass\x0D\n| ABC |\x0D\n"

manyHeaders =
    "X: 1\r\nT: Skänklåt efter Brittas Hans\r\nR: Skänklåt\r\nZ: Brian O'Connor, 11/7/2016\r\nL: 1/16\r\nO: Bjorsa\r\nM: 4/4\r\nK:Gmaj\r\n| ABC |\r\n"

-- notes
fNatural :: AbcNote
fNatural =
    { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

-- key signatures
gMajor :: KeySignature
gMajor =
    { pitchClass: G, accidental: Natural, mode: Major }


gMinor :: KeySignature
gMinor =
    { pitchClass: G, accidental: Natural, mode: Minor }


cMajor :: KeySignature
cMajor =
    { pitchClass: C, accidental: Natural, mode: Major }


dMajor :: KeySignature
dMajor =
    { pitchClass: D, accidental: Natural, mode: Major }


fMajor :: KeySignature
fMajor =
    { pitchClass: F, accidental: Natural, mode: Major }

fMajorM :: ModifiedKeySignature
fMajorM =
    { keySignature: fMajor, modifications: Nil }
