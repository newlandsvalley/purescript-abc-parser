module Test.Notation (notationSuite) where

import Prelude (Unit, discard, show, ($), (<>), (==))
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), intersect, length, (:))
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..))
import Data.Abc.Parser (parse)
import Data.Abc (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Pitch(..), KeySet, Mode(..), AbcNote, AbcTune)
import Data.Abc.Notation
import Data.Abc.Accidentals as Accidentals

import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert

assertOkTitle :: forall e. String -> String -> Test e
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

assertOkKeySig :: forall e. String -> ModifiedKeySignature -> Test e
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

assertOkMeter :: forall e. String -> (Tuple Int Int) -> Test e
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

assertOkNoteLen :: forall e. String -> Rational -> Test e
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

assertNoHeader :: forall e h. String -> (AbcTune -> Maybe h) -> Test e
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

assertHeaderCount :: forall e. Int -> String ->  Test e
assertHeaderCount expectedCount source =
  case parse source of
    Right tune ->
      Assert.equal expectedCount (length tune.headers)

    _ ->
      failure "parse error"

assertEquivalentKeys :: forall e. KeySet -> KeySet -> Test e
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

notationSuite :: forall t. Free (TestF t) Unit
notationSuite = do
   lookupSuite
   headerSuite
   majorModeSuite
   minorModeSuite
   klezmerModeSuite
   otherModeSuite
   modalKeySigNormalisationSuite
   keySuite

headerSuite :: forall t. Free (TestF t) Unit
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


scaleSuite :: forall t. Free (TestF t) Unit
scaleSuite =
  suite "scales" do
    test "G Major" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: G, accidental: Natural, mode: Major })
        (Nil)

majorModeSuite :: forall t. Free (TestF t) Unit
majorModeSuite =
  suite "major mode" do
    test "G Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Major })
        (Pitch  { pitchClass: F, accidental: Sharp} : Nil)
    test "Ab Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
        : Pitch { pitchClass: E, accidental: Flat }
        : Pitch { pitchClass: A, accidental: Flat }
        : Pitch { pitchClass: D, accidental: Flat } : Nil )
    test "A Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Major })
        ( Pitch  { pitchClass: C, accidental: Sharp}
        : Pitch  { pitchClass: F, accidental: Sharp}
        : Pitch  { pitchClass: G, accidental: Sharp}
        : Nil)
    test "Bb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Flat, mode: Major })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat } : Nil )
    test "C Major" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Major })
        (Nil)
    test "B Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Natural, mode: Major })
        (Pitch  { pitchClass: C, accidental: Sharp}
        : Pitch  { pitchClass: F, accidental: Sharp}
        : Pitch  { pitchClass: G, accidental: Sharp}
        : Pitch  { pitchClass: D, accidental: Sharp}
        : Pitch  { pitchClass: A, accidental: Sharp}
        : Nil)
    test "Db Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Flat, mode: Major })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat }
        : Pitch  { pitchClass: A, accidental: Flat }
        : Pitch  { pitchClass: D, accidental: Flat }
        : Pitch  { pitchClass: G, accidental: Flat }
        : Nil )
    test "D Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Natural, mode: Major })
        (Pitch  { pitchClass: C, accidental: Sharp}
        : Pitch  { pitchClass: F, accidental: Sharp}
        : Nil)
    test "Eb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Flat, mode: Major })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat }
        : Pitch  { pitchClass: A, accidental: Flat }
        : Nil )
    test "E Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Natural, mode: Major })
        (Pitch  { pitchClass: C, accidental: Sharp}
        : Pitch  { pitchClass: F, accidental: Sharp}
        : Pitch  { pitchClass: G, accidental: Sharp}
        : Pitch  { pitchClass: D, accidental: Sharp}
        : Nil)
    test "F Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Natural, mode: Major })
        (Pitch  { pitchClass: B, accidental: Flat} : Nil)
    test "F# Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Sharp, mode: Major })
        (Pitch  { pitchClass: C, accidental: Sharp}
        : Pitch  { pitchClass: F, accidental: Sharp}
        : Pitch  { pitchClass: G, accidental: Sharp}
        : Pitch  { pitchClass: D, accidental: Sharp}
        : Pitch  { pitchClass: A, accidental: Sharp}
        : Pitch  { pitchClass: E, accidental: Sharp}
        : Nil)
    test "Gb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Flat, mode: Major })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat }
        : Pitch  { pitchClass: A, accidental: Flat }
        : Pitch  { pitchClass: D, accidental: Flat }
        : Pitch  { pitchClass: G, accidental: Flat }
        : Pitch  { pitchClass: C, accidental: Flat }
        : Nil )

minorModeSuite :: forall t. Free (TestF t) Unit
minorModeSuite =
  suite "minor modes" do
    test "A Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Minor })
        (Nil)
    test "G Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Minor })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat } : Nil )

klezmerModeSuite :: forall t. Free (TestF t) Unit
klezmerModeSuite =
  suite "klezmer modes" do
    test "D Phrygian with sharpened f" do
      assertEquivalentKeys
        (modifiedKeySet  { keySignature: { pitchClass: D, accidental: Natural, mode: Phrygian },
             modifications: ( Pitch  { pitchClass: F, accidental: Sharp } : Nil ) })
          ( Pitch  { pitchClass: B, accidental: Flat }
          : Pitch  { pitchClass: E, accidental: Flat }
          : Pitch  { pitchClass: F, accidental: Sharp }
          : Nil )

otherModeSuite :: forall t. Free (TestF t) Unit
otherModeSuite =
  suite "other modes" do
    test "C Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Dorian })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch { pitchClass: E, accidental: Flat } : Nil )
    test "D Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Natural, mode: Dorian })
        (Nil)
    test "C Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Phrygian })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat }
        : Pitch  { pitchClass: A, accidental: Flat }
        : Pitch  { pitchClass: D, accidental: Flat } : Nil )
    test "E Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Natural, mode: Phrygian })
        (Nil)
    test "C Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Lydian })
        (Pitch  { pitchClass: F, accidental: Sharp} : Nil)
    test "F Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Natural, mode: Lydian })
        (Nil)
    test "C Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Mixolydian })
        (Pitch  { pitchClass: B, accidental: Flat} : Nil)
    test "G Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Mixolydian })
        (Nil)
    test "C Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Aeolian })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat }
        : Pitch  { pitchClass: A, accidental: Flat }
        : Nil )
    test "A Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Aeolian })
        (Nil)
    test "C Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Locrian })
        ( Pitch  { pitchClass: B, accidental: Flat }
        : Pitch  { pitchClass: E, accidental: Flat }
        : Pitch  { pitchClass: A, accidental: Flat }
        : Pitch  { pitchClass: D, accidental: Flat }
        : Pitch  { pitchClass: G, accidental: Flat }
        : Nil )
    test "B Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Natural, mode: Locrian })
        (Nil)
    test "C Ionian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Ionian })
        (Nil)

modalKeySigNormalisationSuite :: forall t. Free (TestF t) Unit
modalKeySigNormalisationSuite =
  suite "modal key sgnature normalisation" do
    test "D Mix" do
      Assert.equal
        ( showKeySig { pitchClass: G, accidental: Natural, mode: Major })
        ( showKeySig $ normaliseModalKey { pitchClass: D, accidental: Natural, mode: Mixolydian })
    test "Bb Dor" do
      Assert.equal
        ( showKeySig { pitchClass: A, accidental: Flat, mode: Major })
        ( showKeySig $ normaliseModalKey { pitchClass: B, accidental: Flat, mode: Dorian })
    test "A Phr" do
      Assert.equal
        ( showKeySig { pitchClass: F, accidental: Natural, mode: Major })
        ( showKeySig $ normaliseModalKey { pitchClass: A, accidental: Natural, mode: Phrygian })
    test "Ab Lyd" do
      Assert.equal
        ( showKeySig { pitchClass: E, accidental: Flat, mode: Major })
        ( showKeySig $ normaliseModalKey { pitchClass: A, accidental: Flat, mode: Lydian })
    test "G# Loc" do
      Assert.equal
        ( showKeySig { pitchClass: A, accidental: Natural, mode: Major })
        ( showKeySig $ normaliseModalKey { pitchClass: G, accidental: Sharp, mode: Locrian })


keySuite :: forall t. Free (TestF t) Unit
keySuite =
  suite "keys" do
    test "D is a sharp key" do
      Assert.assert "not a sharp key" (isCOrSharpKey dMajor)
    test "C is an (honourary) sharp key" do
      Assert.assert "not a sharp key" (isCOrSharpKey cMajor)
    test "F is not a sharp key" do
      Assert.assertFalse "is a sharp key" (isCOrSharpKey fMajor)
    test "Gm is not a sharp key" do
      Assert.assertFalse "is a sharp key" (isCOrSharpKey gMinor)

-- | really belongs in an Accidentals test suite
lookupSuite :: forall t. Free (TestF t) Unit
lookupSuite =
  suite "lookups" do
    test "f in G Major" do
      Assert.equal
          (Just Sharp)
          (Accidentals.implicitInKeySet F
             (modifiedKeySet { keySignature: gMajor, modifications: Nil }))

    test "f in G Major" do
      Assert.equal
        (Nothing)
        (Accidentals.implicitInKeySet F
           (modifiedKeySet { keySignature: cMajor, modifications: Nil }))


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
