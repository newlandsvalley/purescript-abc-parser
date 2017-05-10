module Test.Transposition (transpositionSuite) where

import Test.Utils
import Test.Unit.Assert as Assert
import Data.Abc.Parser (parse)
import Data.Abc.Canonical (abcNote, fromTune)
import Data.Abc.Transposition
import Data.Abc (AbcTune, AbcNote, Accidental(..), KeyAccidental(..), PitchClass(..), Mode(..), ModifiedKeySignature)
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)
import Data.Rational (fromInt)
import Prelude (Unit, (>>=), ($), bind, discard, map, negate, show)
import Test.Unit (Test, TestF, failure, suite, test)

assertTranspositionMatches :: forall e. String -> KeyAccidental -> String -> Test e
assertTranspositionMatches s targetka target =
  case parse s of
    Right tune ->
      Assert.equal target $ fromTune (transposeTo targetka tune)
    Left _ ->
      failure "unexpected parse error"

transpositionSuite :: forall t. Free (TestF t) Unit
transpositionSuite = do
  suite "transposition" do
    keySuite
    noteSuite
    phraseSuite
    keyChangeSuite

keySuite :: forall t. Free (TestF t) Unit
keySuite = do
  suite "keys" do
    test "C to G#" do
      Assert.equal
        (Right 8)
        (keyDistance gSharpMajor cMajor)
    test "G# to Bb" do
      Assert.equal
        (Right 2)
        (keyDistance bFlat gSharpMajor)
    test "Bb to G#" do
      Assert.equal
        (Right (-2))
        (keyDistance gSharpMajor bFlat )
    test "Bm to Am" do
      Assert.equal
        (Left "incompatible modes")
        (keyDistance bFlatDorian cMajor)

-- | Transposition within the contexts of given keys
-- | This is another example of where I find PureScript awkward
-- | no derived instances are available for AbcNote because it's defined
-- | as simply as possible as a record.  However we can 'show' an AbcNote
-- | because it's there in Canonical.  So we compare the stringified versions.
noteSuite :: forall t. Free (TestF t) Unit
noteSuite = do
  suite "notes" do
    test "F in FMaj to GMaj" do
      Assert.equal
        (map abcNote (Right g))
        (map abcNote (transposeNote gMajor fMajor f))
    test "FNat in GMaj to FMaj" do
      Assert.equal
        (map abcNote (Right eb))
        (map abcNote (transposeNote fMajor gMajor fnat))
    test "C# in AMaj to GMaj" do
      Assert.equal
        (map abcNote (Right b))
        (map abcNote (transposeNote gMajor aMajor cs))
    test "C# in GMaj to AMaj" do
      Assert.equal
        (map abcNote (Right ds))
        (map abcNote (transposeNote aMajor gMajor cs))
    test "G# in Amin to FMin" do
      Assert.equal
        (map abcNote (Right enat))
        (map abcNote (transposeNote fMinor aMinor gs))
    test "B in DMaj to CMaj" do
      Assert.equal
        (map abcNote (Right a))
        (map abcNote (transposeNote cMajor dMajor b))

phraseSuite :: forall t. Free (TestF t) Unit
phraseSuite = do
  suite "phrases" do
    test "C phrase to D phrase" do
      assertTranspositionMatches
        cPhrase
        -- dMajor
        (KeyAccidental { pitchClass : D, accidental : Natural })
        dPhrase
    test "D phrase to C phrase" do
      assertTranspositionMatches
        dPhrase
        -- cMajor
        (KeyAccidental { pitchClass : C, accidental : Natural })
        cPhrase
    test "C phrase to F phrase" do
      assertTranspositionMatches
        cPhrase
        -- fMajor
        (KeyAccidental { pitchClass : F, accidental : Natural })
        fPhrase
    test "Gm phrase to Dm phrase" do
      assertTranspositionMatches
        gmPhrase
        -- dMinor
        (KeyAccidental { pitchClass : D, accidental : Natural })
        dmPhrase
    test "Gm phrase with in-bar accidental" do
      assertTranspositionMatches
        gmPhraseLocal
        -- dMinor
        (KeyAccidental { pitchClass : D, accidental : Natural })
        dmPhrase
    test "Dm phrase to Gm phrase" do
      assertTranspositionMatches
        dmPhrase
        -- gMinor
        (KeyAccidental { pitchClass : G, accidental : Natural })
        gmPhraseLocal
    test "Bm phrase to Em phrase" do
      assertTranspositionMatches
        bmPhrase
        -- eMinor
        (KeyAccidental { pitchClass : E, accidental : Natural })
        emPhrase
    test "Am phrase to Fm phrase" do
      assertTranspositionMatches
        amPhrase
        -- fMinor
        (KeyAccidental { pitchClass : F, accidental : Natural })
        fmPhrase
    test "Am phrase to F#m phrase" do
      assertTranspositionMatches
        amPhrase0
        -- fSharpMinor
        (KeyAccidental { pitchClass : F, accidental : Sharp })
        fsharpmPhrase0
    test "identity transposition" do
      assertTranspositionMatches
        dmPhrase
        -- dMinor
        (KeyAccidental { pitchClass : D, accidental : Natural })
        dmPhrase
    test "Cm phrase to Am phrase" do
      assertTranspositionMatches
        cmPhrase1
        -- aMinor
        (KeyAccidental { pitchClass : A, accidental : Natural })
        amPhrase1High


keyChangeSuite :: forall t. Free (TestF t) Unit
keyChangeSuite = do
  suite "key changes" do
    test "key change Bm to Am" do
      assertTranspositionMatches
        keyChangeBm
        -- aMinor
        (KeyAccidental { pitchClass : A, accidental : Natural })
        keyChangeAm
    test "key change Am to Bm" do
      assertTranspositionMatches
        keyChangeAm
        -- bMinor
        (KeyAccidental { pitchClass : B, accidental : Natural })
        keyChangeBm
    test "key change Bm to Em" do
      assertTranspositionMatches
        keyChangeBm
        -- eMinor
        (KeyAccidental { pitchClass : E, accidental : Natural })
        keyChangeEmHigh
    test "key change Em to Bm" do
      assertTranspositionMatches
        keyChangeEm
        -- bMinor
        (KeyAccidental { pitchClass : B, accidental : Natural })
        keyChangeBm
    test "key change Bm to C#m" do
      assertTranspositionMatches
        keyChangeBm
        -- cSharpMinor
        (KeyAccidental { pitchClass : C, accidental : Sharp })
        keyChangeCSharpmHigh
    test "key change C#m to Bm" do
      assertTranspositionMatches
        keyChangeCSharpm
        -- bMinor
        (KeyAccidental { pitchClass : B, accidental : Natural })
        keyChangeBm
    test "key change Bm to Am inline" do
      assertTranspositionMatches
        keyChangeBmInline
        -- aMinor
        (KeyAccidental { pitchClass : A, accidental : Natural })
        keyChangeAmInline


-- note C Sharp and D Sharp are in octave 5 all the other notes are in octave 4

buildKeySig :: PitchClass -> Maybe Accidental -> Mode -> ModifiedKeySignature
buildKeySig pc acc mode =
  { keySignature:  { pitchClass: pc, accidental: acc, mode: mode }, modifications: Nil }

cs :: AbcNote
cs =
  { pitchClass: C, accidental: Just Sharp, octave: 5, duration: fromInt 1, tied: false }


ds :: AbcNote
ds =
  { pitchClass: D, accidental: Just Sharp, octave: 5, duration: fromInt 1, tied: false }


eb :: AbcNote
eb =
  { pitchClass: E, accidental: Just Flat, octave: 4, duration: fromInt 1, tied: false }


enat :: AbcNote
enat =
  { pitchClass: E, accidental: Just Natural, octave: 4, duration: fromInt 1, tied: false }


b ::AbcNote
b =
  { pitchClass: B, accidental: Nothing, octave: 4, duration: fromInt 1, tied: false }


bnat :: AbcNote
bnat =
  { pitchClass: B, accidental: Just Natural, octave: 4, duration: fromInt 1, tied: false }


f :: AbcNote
f =
  { pitchClass: F, accidental: Nothing, octave: 4, duration: fromInt 1, tied: false }


fnat :: AbcNote
fnat =
  { pitchClass: F, accidental: Just Natural, octave: 4, duration: fromInt 1, tied: false }


g :: AbcNote
g =
  { pitchClass: G, accidental: Nothing, octave: 4, duration: fromInt 1, tied: false }


gs :: AbcNote
gs =
  { pitchClass: G, accidental: Just Sharp, octave: 4, duration: fromInt 1, tied: false }

a :: AbcNote
a =
  { pitchClass: A, accidental: Nothing, octave: 4, duration: fromInt 1, tied: false }


fMajor :: ModifiedKeySignature
fMajor =
  buildKeySig F Nothing Major


fMinor :: ModifiedKeySignature
fMinor =
  buildKeySig F Nothing Minor


fSharpMinor :: ModifiedKeySignature
fSharpMinor =
  buildKeySig F (Just Sharp) Minor


gMajor :: ModifiedKeySignature
gMajor =
  buildKeySig G Nothing Major

gMinor :: ModifiedKeySignature
gMinor =
  buildKeySig G Nothing Minor


aMajor :: ModifiedKeySignature
aMajor =
  buildKeySig A Nothing Major


aMinor :: ModifiedKeySignature
aMinor =
  buildKeySig A Nothing Minor


bMinor :: ModifiedKeySignature
bMinor =
  buildKeySig B Nothing Minor


gSharpMajor :: ModifiedKeySignature
gSharpMajor =
  buildKeySig G (Just Sharp) Major


cMajor :: ModifiedKeySignature
cMajor =
  buildKeySig C Nothing Major


cSharpMinor :: ModifiedKeySignature
cSharpMinor =
  buildKeySig C (Just Sharp) Minor


dMajor :: ModifiedKeySignature
dMajor =
  buildKeySig D Nothing Major

dMinor :: ModifiedKeySignature
dMinor =
  buildKeySig D Nothing Minor


eMinor :: ModifiedKeySignature
eMinor =
  buildKeySig E Nothing Minor


bFlatDorian :: ModifiedKeySignature
bFlatDorian =
  { keySignature:  { pitchClass: B, accidental:  Just Flat, mode: Dorian }, modifications: Nil }

bFlat :: ModifiedKeySignature
bFlat =
  buildKeySig B (Just Flat) Major


cPhrase =
    "K: CMajor\x0D\n| AB (3cde [fg] |\x0D\n"


dPhrase =
    "K: DMajor\x0D\n| Bc (3def [ga] |\x0D\n"


fPhrase =
    "K: FMajor\x0D\n| de (3fga [bc'] |\x0D\n"


gmPhrase =
    "K: GMinor\x0D\n| G3A B6 Ac |\x0D\n B2AG ^FGA^F D4\x0D\n"


gmPhraseLocal =
    "K: GMinor\x0D\n| G3A B6 Ac |\x0D\n B2AG ^FGAF D4\x0D\n"



-- second F implicitly sharpened


dmPhrase =
    "K: DMinor\x0D\n| D3E F6 EG |\x0D\n F2ED ^CDEC A,4\x0D\n"


bmPhrase =
    "K: BMinor\x0D\n| B4 A4 B4 | c2d2 e2dc c2d2 |\x0D\n"


emPhrase =
    "K: EMinor\x0D\n| e4 d4 e4 | f2g2 a2gf f2g2 |\x0D\n"


amPhrase0 =
    "K: AMinor\x0D\n| edcB A2E2 C2E2 | A^GAB cBcd e4 |\x0D\n"


fsharpmPhrase0 =
    "K: F#Minor\x0D\n| cBAG F2C2 A,2C2 | F=F^FG AGAB c4 |\x0D\n"


amPhrase1High =
    "K: AMinor\x0D\n| c'2ba ^gabg e4 |\x0D\n"


amPhrase1 =
    "K: AMinor\x0D\n| c2BA ^GABG E4 |\x0D\n"


cmPhrase1 =
    "K: CMinor\x0D\n| e2dc =BcdB G4 |\x0D\n"


fmPhrase1 =
    "K: FMinor\x0D\n| A2GF =EFGE C4 |\x0D\n"


amPhrase =
    "K: AMinor\x0D\n| e2ef g2gf e2ed | c2ce d2dB c4 |\x0D\n"


fmPhrase =
    "K: FMinor\x0D\n| c2c^c e2ec =c2cB | A2Ac B2BG A4 |\x0D\n"


keyChangeBm =
    "K: BMinor\x0D\n| B4 A4 B4 | d2f2 e2dc c2d2 |\x0D\nK: F#Minor\x0D\n| f4 e4 f4 | g2a2 b2ag g2a2 |\x0D\n"


keyChangeAm =
    "K: AMinor\x0D\n| A4 G4 A4 | c2e2 d2cB B2c2 |\x0D\nK: EMinor\x0D\n| e4 d4 e4 | f2g2 a2gf f2g2 |\x0D\n"


keyChangeEm =
    "K: EMinor\x0D\n| E4 D4 E4 | G2B2 A2GF F2G2 |\x0D\nK: BMinor\x0D\n| B4 A4 B4 | c2d2 e2dc c2d2 |\x0D\n"


keyChangeEmHigh =
    "K: EMinor\x0D\n| e4 d4 e4 | g2b2 a2gf f2g2 |\x0D\nK: BMinor\x0D\n| b4 a4 b4 | c'2d'2 e'2d'c' c'2d'2 |\x0D\n"


keyChangeCSharpm =
    "K: C#Minor\x0D\n| C4 B,4 C4 | E2G2 F2ED D2E2 |\x0D\nK: G#Minor\x0D\n| G4 F4 G4 | A2B2 c2BA A2B2 |\x0D\n"


keyChangeCSharpmHigh =
    "K: C#Minor\x0D\n| c4 B4 c4 | e2g2 f2ed d2e2 |\x0D\nK: G#Minor\x0D\n| g4 f4 g4 | a2b2 c'2ba a2b2 |\x0D\n"


keyChangeBmInline =
    "K: BMinor\x0D\n| B4 A4 B4 | d2f2 e2dc c2d2 | [K: F#Minor] f4 e4 f4 | g2a2 b2ag g2a2 |\x0D\n"


keyChangeAmInline =
    "K: AMinor\x0D\n| A4 G4 A4 | c2e2 d2cB B2c2 | [K: EMinor] e4 d4 e4 | f2g2 a2gf f2g2 |\x0D\n"
