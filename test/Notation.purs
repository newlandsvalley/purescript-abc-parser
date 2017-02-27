module Test.Notation (notationSuite) where

import Prelude
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), intersect, length, null, (:))
import Data.Rational (fromInt)
import Data.Map (keys)
import Abc (parse)
import Abc.ParseTree (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), KeyAccidental(..), KeySet, Mode(..), AbcNote, AbcTune)
import Music.Notation

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
      let
        headerMap =
          getHeaderMap tune

        count =
          length $ keys $ headerMap
      in
        Assert.equal expectedCount count

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

notationSuite :: forall t. Free (TestF t) Unit
notationSuite = do
   headerSuite
   majorModeSuite
   minorModeSuite
   otherModeSuite
   keySuite
   {-
   klezmerModeSuite
   -}

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
     assertHeaderCount 7 manyHeaders


scaleSuite :: forall t. Free (TestF t) Unit
scaleSuite =
  suite "scales" do
    test "G Major" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: G, accidental: Nothing, mode: Major })
        (Nil)

majorModeSuite :: forall t. Free (TestF t) Unit
majorModeSuite =
  suite "major mode" do
    test "G Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Nothing, mode: Major })
        (KeyAccidental { pitchClass: F, accidental: Sharp} : Nil)
    test "Ab Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Just Flat, mode: Major })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : KeyAccidental { pitchClass: D, accidental: Flat } : Nil )
    test "A Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Nothing, mode: Major })
        (KeyAccidental { pitchClass: C, accidental: Sharp}
        : KeyAccidental { pitchClass: F, accidental: Sharp}
        : KeyAccidental { pitchClass: G, accidental: Sharp}
        : Nil)
    test "Bb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Just Flat, mode: Major })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat } : Nil )
    test "C Major" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Major })
        (Nil)
    test "B Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Nothing, mode: Major })
        (KeyAccidental { pitchClass: C, accidental: Sharp}
        : KeyAccidental { pitchClass: F, accidental: Sharp}
        : KeyAccidental { pitchClass: G, accidental: Sharp}
        : KeyAccidental { pitchClass: D, accidental: Sharp}
        : KeyAccidental { pitchClass: A, accidental: Sharp}
        : Nil)
    test "Db Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Just Flat, mode: Major })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : KeyAccidental { pitchClass: D, accidental: Flat }
        : KeyAccidental { pitchClass: G, accidental: Flat }
        : Nil )
    test "D Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Nothing, mode: Major })
        (KeyAccidental { pitchClass: C, accidental: Sharp}
        : KeyAccidental { pitchClass: F, accidental: Sharp}
        : Nil)
    test "Eb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Just Flat, mode: Major })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : Nil )
    test "E Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Nothing, mode: Major })
        (KeyAccidental { pitchClass: C, accidental: Sharp}
        : KeyAccidental { pitchClass: F, accidental: Sharp}
        : KeyAccidental { pitchClass: G, accidental: Sharp}
        : KeyAccidental { pitchClass: D, accidental: Sharp}
        : Nil)
    test "F Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Nothing, mode: Major })
        (KeyAccidental { pitchClass: B, accidental: Flat} : Nil)
    test "F# Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Just Sharp, mode: Major })
        (KeyAccidental { pitchClass: C, accidental: Sharp}
        : KeyAccidental { pitchClass: F, accidental: Sharp}
        : KeyAccidental { pitchClass: G, accidental: Sharp}
        : KeyAccidental { pitchClass: D, accidental: Sharp}
        : KeyAccidental { pitchClass: A, accidental: Sharp}
        : KeyAccidental { pitchClass: E, accidental: Sharp}
        : Nil)
    test "Gb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Just Flat, mode: Major })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : KeyAccidental { pitchClass: D, accidental: Flat }
        : KeyAccidental { pitchClass: G, accidental: Flat }
        : KeyAccidental { pitchClass: C, accidental: Flat }
        : Nil )

minorModeSuite :: forall t. Free (TestF t) Unit
minorModeSuite =
  suite "minor modes" do
    test "A Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Nothing, mode: Minor })
        (Nil)
    test "G Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Nothing, mode: Minor })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat } : Nil )

klezmerModeSuite :: forall t. Free (TestF t) Unit
klezmerModeSuite =
  suite "klezmer modes" do
    test "D Phrygian with sharpened f" do
      assertEquivalentKeys
        (modifiedKeySet  { keySignature: { pitchClass: D, accidental: Nothing, mode: Phrygian },
             modifications: ( KeyAccidental { pitchClass: F, accidental: Sharp } : Nil ) })
          ( KeyAccidental { pitchClass: B, accidental: Flat }
          : KeyAccidental { pitchClass: E, accidental: Flat }
          : KeyAccidental { pitchClass: F, accidental: Sharp }
          : Nil )

otherModeSuite :: forall t. Free (TestF t) Unit
otherModeSuite =
  suite "other modes" do
    test "C Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Dorian })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat } : Nil )
    test "D Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Nothing, mode: Dorian })
        (Nil)
    test "C Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Phrygian })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : KeyAccidental { pitchClass: D, accidental: Flat } : Nil )
    test "E Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Nothing, mode: Phrygian })
        (Nil)
    test "C Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Lydian })
        (KeyAccidental { pitchClass: F, accidental: Sharp} : Nil)
    test "F Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Nothing, mode: Lydian })
        (Nil)
    test "C Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Mixolydian })
        (KeyAccidental { pitchClass: B, accidental: Flat} : Nil)
    test "G Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Nothing, mode: Mixolydian })
        (Nil)
    test "C Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Aeolian })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : Nil )
    test "A Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Nothing, mode: Aeolian })
        (Nil)
    test "C Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Locrian })
        ( KeyAccidental { pitchClass: B, accidental: Flat }
        : KeyAccidental { pitchClass: E, accidental: Flat }
        : KeyAccidental { pitchClass: A, accidental: Flat }
        : KeyAccidental { pitchClass: D, accidental: Flat }
        : KeyAccidental { pitchClass: G, accidental: Flat }
        : Nil )
    test "B Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Nothing, mode: Locrian })
        (Nil)
    test "C Ionian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Nothing, mode: Ionian })
        (Nil)

keySuite :: forall t. Free (TestF t) Unit
keySuite =
  suite "keys" do
    test "D is a sharp key" do
      Assert.assert "not a sharp key" (isCOrSharpKey dMajor)
    test "C is an (honourary) key" do
      Assert.assert "not a sharp key" (isCOrSharpKey cMajor)
    test "F is not a sharp key" do
      Assert.assertFalse "is a sharp key" (isCOrSharpKey fMajor)


-- headers in sample ABC tunes
keyedTune =
    "K: FMajor\x0D\n| ABC |\x0D\n"

titledTune =
    "T: Gamal Reinlender\x0D\n| ABC |\x0D\n"

doublyTitledTune =
    "T: Nancy Dawson\x0D\nT: Piss Upon the Grass\x0D\n| ABC |\x0D\n"

manyHeaders =
    "X: 1\x0D\nT: Skänklåt efter Brittas Hans\x0D\nR: Skänklåt\x0D\nZ: Brian O'Connor, 11/7/2016\x0D\nO: Bjorsa\x0D\nM: 4/4\x0D\nK:Gmaj\x0D\n| ABC |\x0D\n"

-- notes
fNatural :: AbcNote
fNatural =
    { pitchClass: F, accidental: Nothing, octave: 4, duration: fromInt 1, tied: false }

-- key signatures
gMajor :: KeySignature
gMajor =
    { pitchClass: G, accidental: Nothing, mode: Major }


gMinor :: KeySignature
gMinor =
    { pitchClass: G, accidental: Nothing, mode: Minor }


cMajor :: KeySignature
cMajor =
    { pitchClass: C, accidental: Nothing, mode: Major }


dMajor :: KeySignature
dMajor =
    { pitchClass: D, accidental: Nothing, mode: Major }


fMajor :: KeySignature
fMajor =
    { pitchClass: F, accidental: Nothing, mode: Major }

fMajorM :: ModifiedKeySignature
fMajorM =
    { keySignature: fMajor, modifications: Nil }
