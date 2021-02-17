module Test.Metadata (metadataSuite) where

import Prelude (Unit, discard, map, show, ($), (<>), (==))
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), head, intersect, length, (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Rational (Rational, (%))
import Data.Tuple (Tuple(..))
import Data.Abc.Parser (parse)
import Data.Abc (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..),
                 BodyPart(..), NoteDuration, Header(..), KeySet, Mode(..),
                 AbcChord, AbcNote, AbcTune)
import Data.Abc.Metadata
import Data.Abc.Canonical (fromTune)

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

assertAllTitles :: String -> List String  -> Test
assertAllTitles source target =
  case parse source of
    Right tune ->
      let
        f :: Header -> String 
        f header = 
          case header of  
            Title title -> title 
            _ -> ""
        titleHeaders =
          getHeaders 'T' tune
        titles = 
          map f titleHeaders
      in
        Assert.equal target titles
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

assertEmptyScore :: Boolean -> String -> Test
assertEmptyScore expected source =
  case parse source of
    Right tune ->
      case (head tune.body) of
        Just (Score bars) ->
          Assert.equal expected (isEmptyStave bars)
        _ ->
          failure "test has no Score BodyPart"
    _ ->
      failure "parse error"

buildThumbnail :: String -> String
buildThumbnail s =
  case parse s of
    Right tune ->
      fromTune $ thumbnail tune
    _ ->
      "parse error"

buildThumbnailNoRepeats :: String -> String
buildThumbnailNoRepeats s =
  case parse s of
    Right tune ->
      fromTune $ removeRepeatMarkers $ thumbnail tune
    _ ->
      "parse error"


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
   scoreSuite
   thumbnailSuite
   utilsSuite

headerSuite :: Free TestF Unit
headerSuite =
  suite "headers" do
   test "get title" do
     assertOkTitle titledTune "Gamal Reinlender"
   test "no title" do
     assertNoHeader keyedTune getTitle
   test "get first of multiple titles" do
     assertOkTitle doublyTitledTune "Nancy Dawson"
   test "get all titles" do
     assertAllTitles doublyTitledTune ("Nancy Dawson" : "Piss Upon the Grass" : Nil)
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

scoreSuite :: Free TestF Unit
scoreSuite =
  suite "score" do
    test "empty score" do
      assertEmptyScore true emptyScore
    test "non empty score" do
      assertEmptyScore false keyedTune

thumbnailSuite :: Free TestF Unit
thumbnailSuite =
  suite "thumbnail" do
    test "with lead-in bar" do
      Assert.equal augustssonThumbnail (buildThumbnail augustsson)
    test "without lead-in bar" do
      Assert.equal fastanThumbnail (buildThumbnail fastan)
    test "remove repeat markers" do
      Assert.equal augustssonThumbnailNoRepeats (buildThumbnailNoRepeats augustsson)


utilsSuite :: Free TestF Unit
utilsSuite =
  suite "utils" do
    test "normalise chord" do
      Assert.equal normalisedChord $ normaliseChord denormalisedChord

-- headers in sample ABC tunes
keyedTune :: String
keyedTune =
    "K: FMajor\x0D\n| ABC |\x0D\n"

titledTune :: String
titledTune =
    "T: Gamal Reinlender\x0D\n| ABC |\x0D\n"

doublyTitledTune :: String
doublyTitledTune =
    "T: Nancy Dawson\x0D\nT: Piss Upon the Grass\x0D\n| ABC |\x0D\n"

manyHeaders :: String
manyHeaders =
    "X: 1\r\nT: Skänklåt efter Brittas Hans\r\nR: Skänklåt\r\nZ: Brian O'Connor, 11/7/2016\r\nL: 1/16\r\nO: Bjorsa\r\nM: 4/4\r\nK:Gmaj\r\n| ABC |\r\n"

emptyScore :: String
emptyScore =
    "| @ # | \\r\n|  |\r\n"

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

augustssonHeaders :: String
augustssonHeaders =
  "X: 1\r\n"
  <> "T: Engelska efter Albert Augustsson\r\n"
  <> "N: From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M: 4/4\r\n"
  <> "R: Engelska\r\n"
  <> "S: Orust\r\n"
  <> "Z: John Watson 24/01/2015\r\n"
  <> "L: 1/8\r\n"
  <> "K: AMajor\r\n"

augustsson :: String
augustsson =
  augustssonHeaders
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

augustssonThumbnail :: String
augustssonThumbnail =
  augustssonHeaders
  <> "A>c|: e2f2 efed | c2a2 e3d |\r\n"

augustssonThumbnailNoRepeats :: String
augustssonThumbnailNoRepeats =
  augustssonHeaders
  <> "A>c| e2f2 efed | c2a2 e3d |\r\n"

fastanHeaders :: String
fastanHeaders =
  "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: FMajor\r\n"
  <> "L: 1/16\r\n"

fastan :: String
fastan =
  fastanHeaders
  <> "| (3A4F4G4 A2B2 | (3:2:4c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
  <> "| (3A4F4G4 A2B2 | (3:2:4c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
  <> "| (3:2:5F4B4cBA2 B2d2 | ge3 c4 A4- | (3:2:5A4B4cBA2 B2d2 | de3 c8- |\r\n"
  <> "| (3:2:5F4B4cBA2 B2d2 | (3:2:4g2a2f4g4 e4- | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"

fastanThumbnail :: String
fastanThumbnail =
  fastanHeaders
  <> "| (3A4F4G4 A2B2 | (3:2:4c2d2B4c4 A2F2 |\r\n"

bnote :: NoteDuration -> AbcNote
bnote duration =
  { pitchClass: B, accidental: Implicit, octave: 4, duration, tied: false }

dnote :: NoteDuration -> AbcNote
dnote duration =
  { pitchClass: D, accidental: Implicit, octave: 4, duration, tied: false }

denormalisedChordNotes :: NonEmptyList AbcNote
denormalisedChordNotes =
  NonEmptyList $ (bnote (1 % 4)) :| ((dnote (1 % 4)) : Nil)

normalisedChordNotes :: NonEmptyList AbcNote
normalisedChordNotes =
  NonEmptyList $ (bnote (3 % 4)) :| ((dnote (3 % 4)) : Nil)

normalisedChord :: AbcChord
normalisedChord =
  { leftSlurs : 0, decorations : Nil, notes : normalisedChordNotes, duration : (1 % 1), rightSlurs : 0 }

denormalisedChord :: AbcChord
denormalisedChord =
  { leftSlurs : 0, decorations : Nil, notes : denormalisedChordNotes, duration : (3 % 1), rightSlurs : 0 }
