module Test.Transposition (transpositionSpec) where

import Effect.Aff (Aff)
import Data.Abc.Parser (parse)
import Data.Abc.Canonical (abcNote, fromTune)
import Data.Abc.Transposition
import Data.Abc (AbcNote, Accidental(..), Pitch(..), PitchClass(..), Mode(..), ModifiedKeySignature)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Map (empty)
import Data.Rational (fromInt)
import Prelude (Unit, discard, map, negate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Utils (buildKeySig)


assertTranspositionMatches :: String -> Pitch -> String -> Aff Unit
assertTranspositionMatches s targetp target =
  case parse s of
    Right tune ->
      target `shouldEqual` (fromTune  (transposeTo targetp tune))
    Left _ ->
      fail "unexpected parse error"

transpositionSpec :: Spec Unit
transpositionSpec = do
  describe "transposition" do
    keySpec
    noteSpec
    phraseSpec
    tuneSpec
    keyChangeSpec

keySpec :: Spec Unit
keySpec = do
  describe "keys" do
    it "moves C to G#" do
      shouldEqual
        (Right 8)
        (keyDistance gSharpMajor cMajor)
    it "moves G# to Bb" do
      shouldEqual
        (Right 2)
        (keyDistance bFlat gSharpMajor)
    it "moves Bb to G#" do
      shouldEqual
        (Right (-2))
        (keyDistance gSharpMajor bFlat)
    it "moves Bm to Am" do
      shouldEqual
        (Left "incompatible modes")
        (keyDistance bFlatDorian cMajor)

-- | Transposition within the contexts of given keys
-- | This is another example of where I find PureScript awkward
-- | no derived instances are available for AbcNote because it's defined
-- | as simply as possible as a record.  However we can 'show' an AbcNote
-- | because it's there in Canonical.  So we compare the stringified versions.
noteSpec :: Spec Unit
noteSpec = do
  describe "notes" do
    it "handles F in FMaj to GMaj" do
      shouldEqual
        (map abcNote (Right g))
        (map abcNote (transposeNote gMajor fMajor f))
    it " handles FNat in GMaj to FMaj" do
      shouldEqual
        (map abcNote (Right eb))
        (map abcNote (transposeNote fMajor gMajor fnat))
    it "handles C# in AMaj to GMaj" do
      shouldEqual
        (map abcNote (Right b))
        (map abcNote (transposeNote gMajor aMajor cs))
    it "handles C# in GMaj to AMaj" do
      shouldEqual
        (map abcNote (Right ds))
        (map abcNote (transposeNote aMajor gMajor cs))
    it "handles G# in Amin to FMin" do
      shouldEqual
        (map abcNote (Right enat))
        (map abcNote (transposeNote fMinor aMinor gs))
    it "handles B in DMaj to CMaj" do
      shouldEqual
        (map abcNote (Right a))
        (map abcNote (transposeNote cMajor dMajor b))
    it "handles C in BMin to EMin" do
      shouldEqual
        (map abcNote (Right f))
        (map abcNote (transposeNote eMinor bMinor c))

phraseSpec :: Spec Unit
phraseSpec = do
  describe "phrases" do
    it "moves C phrase to D phrase" do
      assertTranspositionMatches
        cPhrase
        -- dMajor
        (Pitch { pitchClass: D, accidental: Natural })
        dPhrase
    it "moves D phrase to C phrase" do
      assertTranspositionMatches
        dPhrase
        -- cMajor
        (Pitch { pitchClass: C, accidental: Natural })
        cPhrase
    it "moves C phrase to F phrase" do
      assertTranspositionMatches
        cPhrase
        -- fMajor
        (Pitch { pitchClass: F, accidental: Natural })
        fPhrase
    it "moves Gm phrase to Dm phrase" do
      assertTranspositionMatches
        gmPhrase
        -- dMinor
        (Pitch { pitchClass: D, accidental: Natural })
        dmPhrase
    it "moves Gm phrase with in-bar accidental" do
      assertTranspositionMatches
        gmPhraseLocal
        -- dMinor
        (Pitch { pitchClass: D, accidental: Natural })
        dmPhrase
    it "moves Dm phrase to Gm phrase" do
      assertTranspositionMatches
        dmPhrase
        -- gMinor
        (Pitch { pitchClass: G, accidental: Natural })
        gmPhraseLocal
    it "moves Bm phrase to Em phrase" do
      assertTranspositionMatches
        bmPhrase
        -- eMinor
        (Pitch { pitchClass: E, accidental: Natural })
        emPhrase
    it "moves Am phrase to Fm phrase" do
      assertTranspositionMatches
        amPhrase
        -- fMinor
        (Pitch { pitchClass: F, accidental: Natural })
        fmPhrase
    it "moves Am phrase to F#m phrase" do
      assertTranspositionMatches
        amPhrase0
        -- fSharpMinor
        (Pitch { pitchClass: F, accidental: Sharp })
        fsharpmPhrase0
    it "respects the identity transposition" do
      assertTranspositionMatches
        dmPhrase
        -- dMinor
        (Pitch { pitchClass: D, accidental: Natural })
        dmPhrase
    it "moves Cm phrase to Am phrase" do
      assertTranspositionMatches
        cmPhrase1
        -- aMinor
        (Pitch { pitchClass: A, accidental: Natural })
        amPhrase1High

-- | test that headers are ordered properly
tuneSpec :: Spec Unit
tuneSpec = do
  describe"tunes" do
    it "moves Bm tune to Am tune" do
      assertTranspositionMatches
        tuneBm
        -- aMinor
        (Pitch { pitchClass: A, accidental: Natural })
        tuneAm

keyChangeSpec :: Spec Unit
keyChangeSpec = do
  describe"key changes" do
    it "changes key Bm to Am" do
      assertTranspositionMatches
        keyChangeBm
        -- aMinor
        (Pitch { pitchClass: A, accidental: Natural })
        keyChangeAm
    it "changes key Am to Bm" do
      assertTranspositionMatches
        keyChangeAm
        -- bMinor
        (Pitch { pitchClass: B, accidental: Natural })
        keyChangeBm
    it "changes key Bm to Em" do
      assertTranspositionMatches
        keyChangeBm
        -- eMinor
        (Pitch { pitchClass: E, accidental: Natural })
        keyChangeEmHigh
    it "changes key Em to Bm" do
      assertTranspositionMatches
        keyChangeEm
        -- bMinor
        (Pitch { pitchClass: B, accidental: Natural })
        keyChangeBm
    it "changes key Bm to C#m" do
      assertTranspositionMatches
        keyChangeBm
        -- cSharpMinor
        (Pitch { pitchClass: C, accidental: Sharp })
        keyChangeCSharpmHigh
    it "changes key C#m to Bm" do
      assertTranspositionMatches
        keyChangeCSharpm
        -- bMinor
        (Pitch { pitchClass: B, accidental: Natural })
        keyChangeBm
    it "changes key Bm to Am inline" do
      assertTranspositionMatches
        keyChangeBmInline
        -- aMinor
        (Pitch { pitchClass: A, accidental: Natural })
        keyChangeAmInline

-- note C Sharp and D Sharp are in octave 5 all the other notes are in octave 4
cs :: AbcNote
cs =
  { pitchClass: C, accidental: Sharp, octave: 5, duration: fromInt 1, tied: false }

ds :: AbcNote
ds =
  { pitchClass: D, accidental: Sharp, octave: 5, duration: fromInt 1, tied: false }

eb :: AbcNote
eb =
  { pitchClass: E, accidental: Flat, octave: 4, duration: fromInt 1, tied: false }

enat :: AbcNote
enat =
  { pitchClass: E, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }

b :: AbcNote
b =
  { pitchClass: B, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

c :: AbcNote
c =
  { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

bnat :: AbcNote
bnat =
  { pitchClass: B, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }

f :: AbcNote
f =
  { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

fnat :: AbcNote
fnat =
  { pitchClass: F, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }

g :: AbcNote
g =
  { pitchClass: G, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

gs :: AbcNote
gs =
  { pitchClass: G, accidental: Sharp, octave: 4, duration: fromInt 1, tied: false }

a :: AbcNote
a =
  { pitchClass: A, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

fMajor :: ModifiedKeySignature
fMajor =
  buildKeySig F Natural Major

fMinor :: ModifiedKeySignature
fMinor =
  buildKeySig F Natural Minor

fSharpMinor :: ModifiedKeySignature
fSharpMinor =
  buildKeySig F Sharp Minor

gMajor :: ModifiedKeySignature
gMajor =
  buildKeySig G Natural Major

gMinor :: ModifiedKeySignature
gMinor =
  buildKeySig G Natural Minor

aMajor :: ModifiedKeySignature
aMajor =
  buildKeySig A Natural Major

aMinor :: ModifiedKeySignature
aMinor =
  buildKeySig A Natural Minor

bMinor :: ModifiedKeySignature
bMinor =
  buildKeySig B Natural Minor

gSharpMajor :: ModifiedKeySignature
gSharpMajor =
  buildKeySig G Sharp Major

cMajor :: ModifiedKeySignature
cMajor =
  buildKeySig C Natural Major

cSharpMinor :: ModifiedKeySignature
cSharpMinor =
  buildKeySig C Sharp Minor

dMajor :: ModifiedKeySignature
dMajor =
  buildKeySig D Natural Major

dMinor :: ModifiedKeySignature
dMinor =
  buildKeySig D Natural Minor

eMinor :: ModifiedKeySignature
eMinor =
  buildKeySig E Natural Minor

bFlatDorian :: ModifiedKeySignature
bFlatDorian =
  { keySignature: { pitchClass: B, accidental: Flat, mode: Dorian }
  , modifications: Nil
  , properties: empty
  }

bFlat :: ModifiedKeySignature
bFlat =
  buildKeySig B Flat Major

cPhrase =
  "K: CMajor\x0D\n| AB (3zde [fg] |\x0D\n"

dPhrase =
  "K: DMajor\x0D\n| Bc (3zef [ga] |\x0D\n"

fPhrase =
  "K: FMajor\x0D\n| de (3zga [bc'] |\x0D\n"

gmPhrase =
  "K: GMinor\x0D\n| G3A B6 Ac |\x0D\n B2AG ^FGA^F D4\x0D\n"

gmPhraseLocal =
  "K: GMinor\x0D\n| G3A B6 Ac |\x0D\n B2AG ^FGAF D4\x0D\n"

-- second F implicitly sharpened

dmPhrase =
  "K: DMinor\x0D\n| D3E F6 EG |\x0D\n F2ED ^CDEC A,4\x0D\n"

bmPhrase =
  "K: BMinor\x0D\n| B4 A4 B4 | c2d2 e2dc {b}(3c2d2e2 |\x0D\n"

emPhrase =
  "K: EMinor\x0D\n| e4 d4 e4 | f2g2 a2gf {e'}(3f2g2a2 |\x0D\n"

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

tuneBm =
  "X: 1\r\nT: title\r\nM: 3/4\r\nK: BMinor\r\n| d e f |\r\n"

tuneAm =
  "X: 1\r\nT: title\r\nM: 3/4\r\nK: AMinor\r\n| c d e |\r\n"
