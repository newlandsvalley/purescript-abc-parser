module Test.Transposition (transpositionSuite) where

import Music.Transposition
import Test.Utils
import Abc.ParseTree (AbcNote, Accidental(..), PitchClass(..), Mode(..), ModifiedKeySignature)
import Control.Monad.Free (Free)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Rational (fromInt)
import Prelude (Unit, bind)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

transpositionSuite :: forall t. Free (TestF t) Unit
transpositionSuite = do
  suite "transposition" do
    keySuite

keySuite :: forall t. Free (TestF t) Unit
keySuite = do
  suite "keys" do
  test "C to G#" do
      Assert.equal
        (Right 8)
        (keyDistance gSharpMajor cMajor)

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
  buildKeySig C (Just Flat) Major


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
