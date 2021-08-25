module Test.KeySignature (keySignatureSuite) where

import Prelude (Unit, discard, negate, show, ($), (<>), (==))
import Control.Monad.Free (Free)
import Data.Maybe (fromMaybe)
import Data.Map (empty)
import Data.List (List(..), head, length, sort, (:))
import Data.Abc (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Pitch(..), KeySet, Mode(..))
import Data.Abc.KeySignature

import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

assertEquivalentKeys :: KeySet -> KeySet -> Test
assertEquivalentKeys actual expected =
  if (length actual == length expected) then
    let
      as = sort actual
      es = sort expected
    in
      Assert.equal es as
  else
    failure $ "non-equivalent key lengths: "
      <> (show actual)
      <> " not equal to: "
      <> (show expected)

{- It's such a pain to provide Eq, Show on what you'd like to be a somple record
   so for testing purposes just collapse tp a string
   Type class instances for type synonyms are disallowed
showKeySig :: KeySignature -> String
showKeySig ks =
  show ks.pitchClass <> show ks.accidental <> show ks.mode
-}

keySignatureSuite :: Free TestF Unit
keySignatureSuite = do
  majorModeSuite
  minorModeSuite
  klezmerModeSuite
  otherModeSuite
  keySuite
  transposeSignatureSuite
  scaleSuite

majorModeSuite :: Free TestF Unit
majorModeSuite =
  suite "major mode" do
    test "G Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Major })
        (Pitch { pitchClass: F, accidental: Sharp } : Nil)
    test "Ab Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Nil
        )
    test "A Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Nil
        )
    test "Bb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Nil
        )
    test "C Major" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Major })
        (Nil)
    test "B Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Sharp }
            : Pitch { pitchClass: A, accidental: Sharp }
            : Nil
        )
    test "Db Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Pitch { pitchClass: G, accidental: Flat }
            : Nil
        )
    test "D Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Nil
        )
    test "Eb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Nil
        )
    test "E Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Sharp }
            : Nil
        )
    test "F Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Natural, mode: Major })
        (Pitch { pitchClass: B, accidental: Flat } : Nil)
    test "F# Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Sharp, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Sharp }
            : Pitch { pitchClass: A, accidental: Sharp }
            : Pitch { pitchClass: E, accidental: Sharp }
            : Nil
        )
    test "Gb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Pitch { pitchClass: G, accidental: Flat }
            : Pitch { pitchClass: C, accidental: Flat }
            : Nil
        )

minorModeSuite :: Free TestF Unit
minorModeSuite =
  suite "minor modes" do
    test "A Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Minor })
        (Nil)
    test "G Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Minor })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Nil
        )

klezmerModeSuite :: Free TestF Unit
klezmerModeSuite =
  suite "klezmer modes" do
    test "D Phrygian with sharpened f" do
      assertEquivalentKeys
        (modifiedKeySet dPhrygianSharpenedF)
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Nil
        )

otherModeSuite :: Free TestF Unit
otherModeSuite =
  suite "other modes" do
    test "C Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Dorian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Nil
        )
    test "D Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Natural, mode: Dorian })
        (Nil)
    test "C Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Phrygian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Nil
        )
    test "E Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Natural, mode: Phrygian })
        (Nil)
    test "C Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Lydian })
        (Pitch { pitchClass: F, accidental: Sharp } : Nil)
    test "F Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Natural, mode: Lydian })
        (Nil)
    test "C Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Mixolydian })
        (Pitch { pitchClass: B, accidental: Flat } : Nil)
    test "G Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Mixolydian })
        (Nil)
    test "C Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Aeolian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Nil
        )
    test "A Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Aeolian })
        (Nil)
    test "C Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Locrian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Pitch { pitchClass: G, accidental: Flat }
            : Nil
        )
    test "B Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Natural, mode: Locrian })
        (Nil)
    test "C Ionian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Ionian })
        (Nil)
    test "D mixolydian equivalent" do
      let
        newks = normaliseModalKey (mix D)
      Assert.equal G newks.pitchClass
      Assert.equal Natural newks.accidental
      Assert.equal Major newks.mode
    test "C mixolydian equivalent" do
      let
        newks = normaliseModalKey (mix C)
      Assert.equal F newks.pitchClass
      Assert.equal Natural newks.accidental
      Assert.equal Major newks.mode
    test "E dorian equivalent" do
      let
        newks = normaliseModalKey (dor E)
      Assert.equal D newks.pitchClass
      Assert.equal Natural newks.accidental
      Assert.equal Major newks.mode

keySuite :: Free TestF Unit
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

transposeSignatureSuite :: Free TestF Unit
transposeSignatureSuite =
  suite "transpose key signatures" do
    test "G major down 1 tone" do
      let
        newks = transposeKeySignatureBy (-2) gMajorM
      Assert.equal F newks.keySignature.pitchClass
      Assert.equal Natural newks.keySignature.accidental
      Assert.equal Major newks.keySignature.mode
    test "F major up 1 tone" do
      let
        newks = transposeKeySignatureBy 2 fMajorM
      Assert.equal G newks.keySignature.pitchClass
      Assert.equal Natural newks.keySignature.accidental
      Assert.equal Major newks.keySignature.mode
    test "C# major down 8 semitones" do
      let
        newks = transposeKeySignatureBy (-8) $ cSharpM Major
      Assert.equal F newks.keySignature.pitchClass
      Assert.equal Natural newks.keySignature.accidental
      Assert.equal Major newks.keySignature.mode
    test "C# minor down 8 semitones" do
      let
        newks = transposeKeySignatureBy (-8) $ cSharpM Minor
      Assert.equal F newks.keySignature.pitchClass
      Assert.equal Natural newks.keySignature.accidental
      Assert.equal Minor newks.keySignature.mode
    test "F major up 8 semitones" do
      let
        newks = transposeKeySignatureBy 8 fMajorM
      Assert.equal C newks.keySignature.pitchClass
      Assert.equal Sharp newks.keySignature.accidental
      Assert.equal Major newks.keySignature.mode
    test "D phrygian sharpened F up 1 tone" do
      let
        newks = transposeKeySignatureBy 2 dPhrygianSharpenedF
        modification = fromMaybe (Pitch { pitchClass: C, accidental: Natural })
          $ head newks.modifications
      Assert.equal E newks.keySignature.pitchClass
      Assert.equal Natural newks.keySignature.accidental
      Assert.equal Phrygian newks.keySignature.mode
      Assert.equal (Pitch { pitchClass: G, accidental: Sharp }) modification
    test "E phrygian sharpened G down 1 tone" do
      let
        newks = transposeKeySignatureBy (-2) ePhrygianSharpenedG
        modification = fromMaybe (Pitch { pitchClass: C, accidental: Natural })
          $ head newks.modifications
      Assert.equal D newks.keySignature.pitchClass
      Assert.equal Natural newks.keySignature.accidental
      Assert.equal Phrygian newks.keySignature.mode
      Assert.equal (Pitch { pitchClass: F, accidental: Sharp }) modification

scaleSuite :: Free TestF Unit
scaleSuite =
  suite "scales" do
    test "C Major" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: C, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Natural }
            : Pitch { pitchClass: D, accidental: Natural }
            : Pitch { pitchClass: E, accidental: Natural }
            : Pitch { pitchClass: F, accidental: Natural }
            : Pitch { pitchClass: G, accidental: Natural }
            : Pitch { pitchClass: A, accidental: Natural }
            : Pitch { pitchClass: B, accidental: Natural }
            : Nil
        )
    test "G Major" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: G, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Natural }
            : Pitch { pitchClass: D, accidental: Natural }
            : Pitch { pitchClass: E, accidental: Natural }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Natural }
            : Pitch { pitchClass: A, accidental: Natural }
            : Pitch { pitchClass: B, accidental: Natural }
            : Nil
        )
    test "E Minor" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: E, accidental: Natural, mode: Minor })
        ( Pitch { pitchClass: C, accidental: Natural }
            : Pitch { pitchClass: D, accidental: Natural }
            : Pitch { pitchClass: E, accidental: Natural }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Natural }
            : Pitch { pitchClass: A, accidental: Natural }
            : Pitch { pitchClass: B, accidental: Natural }
            : Nil
        )
    test "B Minor" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: B, accidental: Natural, mode: Minor })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Natural }
            : Pitch { pitchClass: E, accidental: Natural }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Natural }
            : Pitch { pitchClass: A, accidental: Natural }
            : Pitch { pitchClass: B, accidental: Natural }
            : Nil
        )
    test "F# Major" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: F, accidental: Sharp, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Sharp }
            : Pitch { pitchClass: E, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Pitch { pitchClass: A, accidental: Sharp }
            : Pitch { pitchClass: B, accidental: Natural }
            : Nil
        )
    test "Gb Major" do
      assertEquivalentKeys
        (diatonicScale { pitchClass: G, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: C, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: F, accidental: Natural }
            : Pitch { pitchClass: G, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: B, accidental: Flat }
            : Nil
        )

-- key signatures
gMajor :: KeySignature
gMajor =
  { pitchClass: G, accidental: Natural, mode: Major }

gMajorM :: ModifiedKeySignature
gMajorM =
  { keySignature: gMajor, modifications: Nil, properties: empty }

gMinor :: KeySignature
gMinor =
  { pitchClass: G, accidental: Natural, mode: Minor }

gMinorM :: ModifiedKeySignature
gMinorM =
  { keySignature: gMinor, modifications: Nil, properties: empty }

cMajor :: KeySignature
cMajor =
  { pitchClass: C, accidental: Natural, mode: Major }

cSharp :: Mode -> KeySignature
cSharp mode =
  { pitchClass: C, accidental: Sharp, mode: mode }

cSharpM :: Mode -> ModifiedKeySignature
cSharpM mode =
  { keySignature: cSharp mode, modifications: Nil, properties: empty }

dMajor :: KeySignature
dMajor =
  { pitchClass: D, accidental: Natural, mode: Major }

mix :: PitchClass -> KeySignature
mix pc =
  { pitchClass: pc, accidental: Natural, mode: Mixolydian }

dor :: PitchClass -> KeySignature
dor pc =
  { pitchClass: pc, accidental: Natural, mode: Dorian }

fMajor :: KeySignature
fMajor =
  { pitchClass: F, accidental: Natural, mode: Major }

fMajorM :: ModifiedKeySignature
fMajorM =
  { keySignature: fMajor, modifications: Nil, properties: empty }

dPhrygianSharpenedF :: ModifiedKeySignature
dPhrygianSharpenedF =
  { keySignature: { pitchClass: D, accidental: Natural, mode: Phrygian }
  , modifications: (Pitch { pitchClass: F, accidental: Sharp } : Nil)
  , properties: empty
  }

ePhrygianSharpenedG :: ModifiedKeySignature
ePhrygianSharpenedG =
  { keySignature: { pitchClass: E, accidental: Natural, mode: Phrygian }
  , modifications: (Pitch { pitchClass: G, accidental: Sharp } : Nil)
  , properties: empty
  }
