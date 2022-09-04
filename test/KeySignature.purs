module Test.KeySignature (keySignatureSpec) where

import Prelude (Unit, discard, negate, pure, show, unit, ($), (<>), (==))
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (empty)
import Data.List (List(..), head, length, sort, (:))
import Data.Abc (AbcTune, PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Pitch(..), KeySet, Mode(..))
import Data.Abc.KeySignature
import Data.Abc.Parser (parse)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


assertEquivalentKeys :: KeySet -> KeySet -> Aff Unit
assertEquivalentKeys actual expected =
  if (length actual == length expected) then
    let
      as = sort actual
      es = sort expected
    in
      es `shouldEqual` as
  else
    fail $ "non-equivalent key lengths: "
      <> (show actual)
      <> " not equal to: "
      <> (show expected)

assertOkKeySig :: String -> ModifiedKeySignature -> Aff Unit
assertOkKeySig source target =
  case parse source of
    Right tune ->
      case (getKeySig tune) of
        Just keySig ->
          target.keySignature.pitchClass `shouldEqual` keySig.keySignature.pitchClass

        _ ->
          fail "no key signature"
    _ ->
      fail "parse error"      

assertNoHeader :: forall h. String -> (AbcTune -> Maybe h) -> Aff Unit
assertNoHeader source getf =
  case parse source of
    Right tune ->
      let
        mtitle =
          getf tune
      in
        case mtitle of
          Just _ ->
            fail "no title expected"
          _ ->
            pure unit

    _ ->
      fail "parse error"

keySignatureSpec :: Spec Unit
keySignatureSpec = do
  headerSpec
  majorModeSpec
  minorModeSpec
  klezmerModeSpec
  otherModeSpec
  keySpec
  transposeSignatureSpec
  scaleSpec

headerSpec :: Spec Unit
headerSpec =
  describe "key signature header" do  
    it "gets key header" do
      assertOkKeySig keyedTune fMajorM
    it "recognizes key header" do
      assertNoHeader unkeyedTune getKeySig

majorModeSpec :: Spec Unit
majorModeSpec =
  describe "major mode" do
    it "handles G Major" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Major })
        (Pitch { pitchClass: F, accidental: Sharp } : Nil)
    it "handles Ab Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Nil
        )
    it "handles A Major" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Nil
        )
    it "handles Bb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Nil
        )
    it "handles C Major" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Major })
        (Nil)
    it "handles B Major" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Sharp }
            : Pitch { pitchClass: A, accidental: Sharp }
            : Nil
        )
    it "handles Db Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Pitch { pitchClass: G, accidental: Flat }
            : Nil
        )
    it "handles D Major" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Nil
        )
    it "handles Eb Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Flat, mode: Major })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Nil
        )
    it "handles E Major" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Natural, mode: Major })
        ( Pitch { pitchClass: C, accidental: Sharp }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Pitch { pitchClass: G, accidental: Sharp }
            : Pitch { pitchClass: D, accidental: Sharp }
            : Nil
        )
    it "handles F Major" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Natural, mode: Major })
        (Pitch { pitchClass: B, accidental: Flat } : Nil)
    it "handles F# Major" do
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
    it "handles Gb Major" do
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

minorModeSpec :: Spec Unit
minorModeSpec =
  describe "minor modes" do
    it "handles A Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Minor })
        (Nil)
    it "handles G Minor" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Minor })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Nil
        )

klezmerModeSpec :: Spec Unit
klezmerModeSpec =
  describe "klezmer modes" do
    it "handles D Phrygian with sharpened f" do
      assertEquivalentKeys
        (modifiedKeySet dPhrygianSharpenedF)
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: F, accidental: Sharp }
            : Nil
        )

otherModeSpec :: Spec Unit
otherModeSpec =
  describe "other modes" do
    it "handles C Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Dorian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Nil
        )
    it "handles D Dorian" do
      assertEquivalentKeys
        (keySet { pitchClass: D, accidental: Natural, mode: Dorian })
        (Nil)
    it "handles C Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Phrygian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Nil
        )
    it "handles E Phrygian" do
      assertEquivalentKeys
        (keySet { pitchClass: E, accidental: Natural, mode: Phrygian })
        (Nil)
    it "handles C Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Lydian })
        (Pitch { pitchClass: F, accidental: Sharp } : Nil)
    it "handles F Lydian" do
      assertEquivalentKeys
        (keySet { pitchClass: F, accidental: Natural, mode: Lydian })
        (Nil)
    it "handles C Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Mixolydian })
        (Pitch { pitchClass: B, accidental: Flat } : Nil)
    it "handles G Mixolydian" do
      assertEquivalentKeys
        (keySet { pitchClass: G, accidental: Natural, mode: Mixolydian })
        (Nil)
    it "handles C Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Aeolian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Nil
        )
    it "handles A Aeolian" do
      assertEquivalentKeys
        (keySet { pitchClass: A, accidental: Natural, mode: Aeolian })
        (Nil)
    it "handles C Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Locrian })
        ( Pitch { pitchClass: B, accidental: Flat }
            : Pitch { pitchClass: E, accidental: Flat }
            : Pitch { pitchClass: A, accidental: Flat }
            : Pitch { pitchClass: D, accidental: Flat }
            : Pitch { pitchClass: G, accidental: Flat }
            : Nil
        )
    it "handles B Locrian" do
      assertEquivalentKeys
        (keySet { pitchClass: B, accidental: Natural, mode: Locrian })
        (Nil)
    it "handles C Ionian" do
      assertEquivalentKeys
        (keySet { pitchClass: C, accidental: Natural, mode: Ionian })
        (Nil)
    it "handles D mixolydian equivalent" do
      let
        newks = normaliseModalKey (mix D)
      G `shouldEqual` newks.pitchClass
      Natural `shouldEqual` newks.accidental
      Major `shouldEqual` newks.mode
    it "handles C mixolydian equivalent" do
      let
        newks = normaliseModalKey (mix C)
      F `shouldEqual` newks.pitchClass
      Natural `shouldEqual` newks.accidental
      Major `shouldEqual`newks.mode
    it "handles E dorian equivalent" do
      let
        newks = normaliseModalKey (dor E)
      D `shouldEqual` newks.pitchClass
      Natural `shouldEqual` newks.accidental
      Major `shouldEqual` newks.mode

keySpec :: Spec Unit
keySpec =
  describe "keys" do
    it "recognizes D is a sharp key" do
      true `shouldEqual` (isCOrSharpKey dMajor)
    it "recognizes C is an (honourary) sharp key" do
      true `shouldEqual` (isCOrSharpKey cMajor)
    it "recognizes F is not a sharp key" do
      false  `shouldEqual` (isCOrSharpKey fMajor)
    it "recognizes Gm is not a sharp key" do
      false `shouldEqual` (isCOrSharpKey gMinor)

transposeSignatureSpec :: Spec Unit
transposeSignatureSpec =
  describe "transpose key signatures" do
    it "moves G major down 1 tone" do
      let
        newks = transposeKeySignatureBy (-2) gMajorM
      F `shouldEqual` newks.keySignature.pitchClass
      Natural `shouldEqual` newks.keySignature.accidental
      Major `shouldEqual` newks.keySignature.mode
    it "moves F major up 1 tone" do
      let
        newks = transposeKeySignatureBy 2 fMajorM
      G `shouldEqual` newks.keySignature.pitchClass
      Natural `shouldEqual` newks.keySignature.accidental
      Major `shouldEqual` newks.keySignature.mode
    it "moves C# major down 8 semitones" do
      let
        newks = transposeKeySignatureBy (-8) $ cSharpM Major
      F `shouldEqual` newks.keySignature.pitchClass
      Natural `shouldEqual` newks.keySignature.accidental
      Major `shouldEqual` newks.keySignature.mode
    it "moves C# minor down 8 semitones" do
      let
        newks = transposeKeySignatureBy (-8) $ cSharpM Minor
      F `shouldEqual` newks.keySignature.pitchClass
      Natural `shouldEqual` newks.keySignature.accidental
      Minor `shouldEqual` newks.keySignature.mode
    it "moves F major up 8 semitones" do
      let
        newks = transposeKeySignatureBy 8 fMajorM
      C `shouldEqual` newks.keySignature.pitchClass
      Sharp `shouldEqual` newks.keySignature.accidental
      Major `shouldEqual` newks.keySignature.mode
    it "moves D phrygian sharpened F up 1 tone" do
      let
        newks = transposeKeySignatureBy 2 dPhrygianSharpenedF
        modification = fromMaybe (Pitch { pitchClass: C, accidental: Natural })
          $ head newks.modifications
      E `shouldEqual` newks.keySignature.pitchClass
      Natural `shouldEqual` newks.keySignature.accidental
      Phrygian `shouldEqual` newks.keySignature.mode
      (Pitch { pitchClass: G, accidental: Sharp }) `shouldEqual` modification
    it "moves E phrygian sharpened G down 1 tone" do
      let
        newks = transposeKeySignatureBy (-2) ePhrygianSharpenedG
        modification = fromMaybe (Pitch { pitchClass: C, accidental: Natural })
          $ head newks.modifications
      D `shouldEqual` newks.keySignature.pitchClass
      Natural `shouldEqual` newks.keySignature.accidental
      Phrygian `shouldEqual` newks.keySignature.mode
      (Pitch { pitchClass: F, accidental: Sharp }) `shouldEqual` modification

scaleSpec :: Spec Unit
scaleSpec =
  describe "scales" do
    it "handles C Major" do
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
    it "handles G Major" do
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
    it "handles E Minor" do
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
    it "handles B Minor" do
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
    it "handles F# Major" do
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
    it "handles Gb Major" do
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

keyedTune :: String
keyedTune =
  "K: FMajor\x0D\n| ABC |\x0D\n"
  
unkeyedTune :: String
unkeyedTune =
  "T: Gamal Reinlender\x0D\n| ABC |\x0D\n"
