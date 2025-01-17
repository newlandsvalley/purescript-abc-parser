module Test.Abc (abcSpec) where

import Prelude

import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.List (length)
import Data.Abc.Parser (parse, parseKeySignature)
import Data.Abc.Canonical (fromTune)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

assertRoundTrip :: String -> Aff Unit
assertRoundTrip s =
  assertCanonical s s

assertCanonical :: String -> String -> Aff Unit
assertCanonical s canonical =
  case (parse s) of
    Right tune ->
      canonical `shouldEqual` (fromTune tune)

    Left err ->
      fail ("parse failed: " <> (show err))

assertParses :: String -> Aff Unit
assertParses s =
  case (parse s) of
    Right _res ->
      pure unit

    Left err ->
      fail ("parse failed: " <> (show err))

assertParseError :: String -> Aff Unit
assertParseError s =
  case (parse s) of
    Right _res ->
      fail "parses when it shouldn't"

    Left _err ->
      pure unit

assertKeySigParses :: String -> Aff Unit
assertKeySigParses s =
  case (parseKeySignature s) of
    Right _res ->
      pure unit

    Left err ->
      fail ("parse failed: " <> (show err))

assertMusicLines :: String -> Int -> Aff Unit
assertMusicLines s target =
  case (parse s) of
    Right tune ->
      target `shouldEqual` (length tune.body)

    Left err ->
      fail ("parse failed: " <> (show err))

abcSpec :: Spec Unit
abcSpec = do
  headerSpec
  noteSpec
  barSpec
  slurSpec
  phrasingSpec
  structureSpec
  ambiguitySpec
  badInputSpec
  keySigSpec

headerSpec :: Spec Unit
headerSpec =
  describe "headers" do
    it "recognizes area" do
      assertRoundTrip "A: London\x0D\n| ABC |\x0D\n"
    it "recognizes book" do
      assertRoundTrip "B: Richie Robinson\x0D\n| ABC |\x0D\n"
    it "recognizes composer" do
      assertRoundTrip "C: Bys-Kalle\x0D\n| ABC |\x0D\n"
    it "recognizes discography" do
      assertRoundTrip "D: 2 Brudetstykke\x0D\n| ABC |\x0D\n"
    it "recognizes file URL" do
      assertRoundTrip "F: http\\\\tradtunedb.org.uk\x0D\n| ABC |\x0D\n"
    it "recognizes group" do
      assertRoundTrip "G: Swåp\x0D\n| ABC |\x0D\n"
    it "recognizes history" do
      assertRoundTrip "H: Learned from AnnbjØrg Lien\x0D\n| ABC |\x0D\n"
    it "recognizes instruction" do
      assertRoundTrip "I: abc-charset UTF-8\x0D\n| ABC |\x0D\n"
    it "recognizes key" do
      assertRoundTrip keyADorian
    it "recognizes key spaced" do
      assertCanonical "K: A Dorian\x0D\n| ABC |\x0D\n" keyADorian
    it "recognizes key with accidental" do
      assertRoundTrip "K: AMinor ^f\x0D\n| ABC |\x0D\n"
    it "recognizes key with unspaced accidental" do
      assertCanonical "K: EMinor^c\x0D\n| ABC |\x0D\n" keyWithAccidental
    it "recognizes simple key" do
      assertCanonical "K: C\x0D\n| ABC |\x0D\n" keyCMajor
    it "recognizes key with trailing space" do
      assertCanonical "K: CMajor \x0D\n| ABC |\x0D\n" keyCMajor
    it "recognizes key with properties" do
      assertRoundTrip "K: GMinor shift=GD\x0D\n| ABC |\x0D\n"
    it "recognizes note length" do
      assertRoundTrip "L: 1/8\x0D\n| ABC |\x0D\n"
    it "recognizes meter" do
      assertRoundTrip "M: 3/4\x0D\n| ABC |\x0D\n"
    it "no meter" do
      assertRoundTrip "M: none\x0D\n| ABC |\x0D\n"
    it "recognizes macro" do
      assertRoundTrip "m: ~g2 = {a}g{f}g\x0D\n| ABC |\x0D\n"
    it "recognizes notes" do
      assertRoundTrip "N: from recording made at Tideswell\x0D\n| ABC |\x0D\n"
    it "recognizes origin" do
      assertRoundTrip "O: Skåne\x0D\n| ABC |\x0D\n"
    it "recognizes parts" do
      assertRoundTrip "P: ((AB)3.(CD)3)2\x0D\n| ABC |\x0D\n"
    it "recognizes tempo" do
      assertRoundTrip standardTempo
    it "recognizes suffixed tempo" do
      assertRoundTrip suffixedTempo
    it "recognizes prefixed tempo" do
      assertCanonical "Q: \"lento\" 1/4=70\x0D\n| ABC |\x0D\n" suffixedTempo
    it "recognizes degenerate tempo" do
      assertCanonical "Q: 120\x0D\n| ABC |\x0D\n" standardTempo
    it "recognizes tempo trailing space" do
      assertCanonical "Q: 1/4=120  \x0D\n| ABC |\x0D\n" standardTempo
    it "recognizes multi-beat tempo" do
      assertRoundTrip "Q: 1/4 3/8 1/4 3/8=40\x0D\n| ABC |\x0D\n"
    it "recognizes remark" do
      assertRoundTrip "r: this is a remark\x0D\n| ABC |\x0D\n"
    it "recognizes rhythm" do
      assertRoundTrip "R: Polska\x0D\n| ABC |\x0D\n"
    it "recognizes source" do
      assertRoundTrip "S: Christine Dyer\x0D\n| ABC |\x0D\n"
    it "recognizes title" do
      assertRoundTrip "T: Engelska efter Albert Augustsson\x0D\n| ABC |\x0D\n"
    it "recognizes user-defined" do
      assertRoundTrip "U: some comment\x0D\n| ABC |\x0D\n"
    it "recognizes simple voice" do
      assertRoundTrip "V: T1\x0D\n| ABC |\x0D\n"
    it "recognizes voice" do
      assertRoundTrip "V: T1 clef=treble-8+8 name=\"Tenore I\" snm=\"T.I\"\x0D\n| ABC |\x0D\n"
    it "recognizes words after" do
      assertRoundTrip "W: doh re mi fa \x0D\n| ABC |\x0D\n"
    -- the words aligned header only appears inline
    it "recognizes words aligned" do
      assertRoundTrip "| ABC |\x0D\nw: doh re mi fa \x0D\n| ABC |\x0D\n"
    it "recognizes reference" do
      assertRoundTrip "X: 125\x0D\n| ABC |\x0D\n"
    it "recognizes degenerate reference with no number" do
      assertRoundTrip "X: \x0D\n| ABC |\x0D\n"
    it "recognizes transcriber" do
      assertRoundTrip "Z: John Watson\x0D\n| ABC |\x0D\n"
    it "recognizes field continuation" do
      assertRoundTrip "R: Polska\x0D\n+: in triplet time\x0D\n| ABC |\x0D\n"
    it "comment" do
      assertRoundTrip "%%TBL:{\"version\":\"beta\",\"type\":\"tune\",\"id\":\"10294\"}\x0D\n| ABC |\x0D\n"
    it "recognizes unsupported header" do
      assertParses "j: custom header\x0D\n| ABC |\x0D\n"
    it "recognizes bracket in header" do
      assertRoundTrip "r: this is a remark [part 1]\x0D\n| ABC |\x0D\n"
    it "recognizes comment in reference number" do
      assertParses "X: 125 % start of header\x0D\n| ABC |\x0D\n"
    it "recognizes comment in key" do
      assertParses "K: C % scale: C major\x0D\n| ABC |\x0D\n"

noteSpec :: Spec Unit
noteSpec =
  describe "note" do
    it "handles single duration" do
      assertRoundTrip "| A |\r\n"
    it "handles doubly implied half duration" do
      assertRoundTrip "| B/ |\r\n"
    it "handles implied half duration" do
      assertCanonical "| B/2 |\r\n" halfNoteCanonical
    it "handles explicit half duration" do
      assertCanonical "| B1/2 |\r\n" halfNoteCanonical
    it "handles quarter duration" do
      assertCanonical "| D// |\r\n" quarterNoteCanonical
    it "handles eighth duration" do
      assertCanonical "| D/// |\r\n" eighthNoteCanonical
    it "handles double duration" do
      assertRoundTrip "| a2 |\r\n"
    it "handles broken rhythm" do
      assertRoundTrip "| A>B C>>D a<b c<<d |\x0D\n"
    it "handles broken rhythm spaced" do
      assertCanonical "| A> B |\x0D\n" "| A>B |\x0D\n"
    it "handles octave" do
      assertRoundTrip "| A,B,,C z2 d'e''f z/ |\x0D\n"
    it "handles tie" do
      assertRoundTrip "| A4- A2 |\x0D\n"
    -- relaxation of the spec for degenerate ties
    it "handles degenerate tie" do
      assertCanonical "| A4 -A2 |\x0D\n" "| A4-A2 |\x0D\n"
    it "handles complex tie" do
      assertRoundTrip "| fg-ga ab-bc|\x0D\n"
    it "handles triplet" do
      assertRoundTrip "| (3efg |\r\n"
    it "handles triplet long form" do
      assertCanonical "| (3:2:3efg |\r\n" "| (3efg |\r\n"
    it "handles triplet intermediate form" do
      assertCanonical "| (3:2efg |\r\n" "| (3efg |\r\n"
    it "handles spaced triplet" do
      assertCanonical "| (3 abc def |\x0D\n" "| (3abc def |\x0D\n"
    it "handles space between notes in triplet" do
      assertCanonical "| (3 a b c def |\x0D\n" "| (3abc def |\x0D\n"
    it "handles triplet with rest" do
      assertRoundTrip "| (3zfg |\r\n"
    it "handles grace note" do
      assertRoundTrip "| {d^f}GA |\x0D\n"
    it "accepts spaces between graces and note" do
      assertCanonical "| {d^f} GA |\x0D\n" "| {d^f}GA |\x0D\n"
    it "handles grace note in tuplet" do
      assertRoundTrip "| (3c{d}fg A |\x0D\n"
    it "handles grace note before tuplet" do
      assertRoundTrip "| {d}(3cfg A |\x0D\n"
    it "handles grace note in broken rhythm pair" do
      assertRoundTrip "| A>{f}B C>>{ef}D |\x0D\n"
    it "handles rest in broken rhythm pair" do
      assertRoundTrip "| A>z z>>D |\x0D\n"
    it "handles double sharp" do
      assertRoundTrip "| ^^C2 |\r\n"
    it "handles sharp" do
      assertRoundTrip "| ^C/ |\r\n"
    it "handles double flat" do
      assertRoundTrip "| __C |\r\n"
    it "handles flat" do
      assertRoundTrip "| _C3/2 |\r\n"
    it "handles natural" do
      assertRoundTrip "| =C3/2 |\r\n"
    it "handles chord symbol" do
      assertRoundTrip "| \"Em\" EG \"Am\" AC |\x0D\n"
    it "handles chords" do
      assertRoundTrip "| [de^f]g [cda]b |\x0D\n"
    it "handles chords with spaced notes" do
      assertCanonical "| [d e ^f ]g [c d a]b |\x0D\n" "| [de^f]g [cda]b |\x0D\n"
    it "handles chord duration" do
      assertRoundTrip "| [cda]4 |\x0D\n"

barSpec :: Spec Unit
barSpec =
  describe "bar lines" do
    it "handles no bars" do
      assertRoundTrip "A B C\r\n"
    it "handles no starting or terminating line" do
      assertRoundTrip "A B C | D E F\r\n"
    it "handles no terminating line" do
      assertRoundTrip "| A B C | D E F\r\n"
    it "handles no bars stave 2" do
      assertRoundTrip "| A B C |\r\n D E F\r\n"
    {-}
    it "from failing transposition test" do
      assertRoundTrip "| G3A B6 Ac |\r\n B2AG ^FGA^F D4\r\n"
    -}
    it "handles repeat" do
      assertRoundTrip "|: A :|\r\n"
    it "handles bracket line" do
      assertRoundTrip "[| A |]\r\n"
    it "handles double colon" do
      assertCanonical "||: A :: c :||\r\n" "||: A :|: c :||\r\n"
    it "handles alternate endings - simple" do
      assertRoundTrip "| A |1 B :|2 c||\r\n"
    it "handles alternate endings - list" do
      assertRoundTrip "| A |1,3 B :|2 c||\r\n"
    it "handles alternate endings - range" do
      assertRoundTrip "| A |1-3 B :|4 c||\r\n"
    it "handles alternate endings - combo" do
      assertRoundTrip "| A |1-3,5 B :|4 c||\r\n"
    it "handles repeat 0" do
      assertRoundTrip "|: ABCD EFGa |1 D4 C4 :|2 c8 |\x0D\n"
    it "handles repeat 1" do
      assertCanonical "|: ABCD EFGa |[1 D4 C4 :|[2 c8 |\x0D\n" repeat
    it "handles repeat 1a" do
      assertRoundTrip "|: ABCD EFGa [|1 D4 C4 :[|2 c8 |]\x0D\n"
    it "handles repeat 2" do
      assertRoundTrip "|: ABCD EFGa [|1 D4 C4 :[|2 c8 |]\x0D\n"
    it "handles repeat 3" do
      assertRoundTrip repeat3
    it "handles repeat 3a" do
      assertCanonical "|: ABCD EFGa :|: c8 |\x0D\n" repeat3
    it "handles repeat 3b" do
      assertRoundTrip "|: ABCD EFGa :||: c8 |\x0D\n"
    it "handles repeat 4" do
      assertRoundTrip "[|2 ABCD EFGa |]: c8 |\x0D\n"
    it "repeat 5" do
      assertRoundTrip "|: ABCD EFGa :|] c8 |\x0D\n"
    it "handles repeat 6" do
      assertRoundTrip "[|2 ABCD EFGa ||: c8 |\x0D\n"
    it "handles repeat 7" do
      assertRoundTrip "| ABCD EFGa :|| c8 |\x0D\n"
    it "handles degenerate repeat 1" do
      assertCanonical "[1 ABCD |\x0D\n" "|1 ABCD |\x0D\n"
    it "handles degenerate repeat 2" do
      assertCanonical "| [1 ABCD |\x0D\n" "| |1 ABCD |\x0D\n"

slurSpec :: Spec Unit
slurSpec =
  describe "slurs" do
    it "handles slur" do
      assertRoundTrip "| (de^f) (cda) |\x0D\n"
    it "handles broken rhythm slurred start" do
      assertRoundTrip "| A>(B C) |\x0D\n"
    it "handles broken rhythm slurred finish" do
      assertRoundTrip "| (B C)>A |\x0D\n"
    it "handles grace note with slur" do
      assertRoundTrip "| {d^f}(GA) |\x0D\n"
    it "handles chord with leading slur" do
      assertRoundTrip "| ([GB] d) |\x0D\n"
    it "handles chord with trailing slur" do
      assertRoundTrip "| (d [GB]) |\x0D\n"
    it "handles slurred tuplet" do
      assertRoundTrip "| ((3def) |\x0D\n"
    -- we throw away the slur if it mistakenly encompasses the operator
    it "handles degenerate slurred broken rhythm start" do
      assertCanonical "| A(>B C) |\x0D\n" "| A>B C) |\x0D\n"
    it "handles degenerate slurred broken rhythm finish" do
      assertCanonical "| (BC>)A |\x0D\n" "| (BC>A |\x0D\n"

{- this test would fail.  We don't allow slurs to span note sequences
   starting with a grace note.  Instead, users should start the slur at
   the first full note
test "degenerate slurred grace" do
  assertParses "| ({d^f}GA) |\x0D\n"
-}

phrasingSpec :: Spec Unit
phrasingSpec =
  describe "phrasing" do
    it "handles articulation" do
      assertRoundTrip "(vA2 | !fz! Ld2).d.f .e.d.c.B A2(A2 | d2).d.f .e.d.c.B A2A2 |\x0D\n"
    it "handles annotation" do
      assertRoundTrip "| \"<(\" \">)\" EG |\x0D\n"
    it "handles decorated note" do
      assertRoundTrip "| !uppermordent! !trill! C |\x0D\n"
    it "handles decorated space" do
      assertRoundTrip "| ABc !coda! y |\x0D\n"
    it "handles decorated chord" do
      assertRoundTrip "| A B C | !uppermordent! [DE] |\r\n"
    it "handles decorated bar" do
      assertRoundTrip "| A B C | D E F !dacapo! |\r\n"

structureSpec :: Spec Unit
structureSpec =
  describe "structure" do
    it "handles ignore" do
      assertParses "| ABC# z2 @def z/ |\x0D\n"
    it "handles typeset space" do
      assertParses "| ABC yz2 defyz/ |\x0D\n"
    it "handles backtick" do
      assertParses "| A``B``C |\x0D\n"
    it "handles inline" do
      assertRoundTrip "| ABC z2 def z/ \x0D\nQ: 1/4=120\x0D\n| ABC z2 def z/ |\x0D\n"
    it "handles inline voice reference" do
      assertRoundTrip "[V: T1]| ABC |\x0D\n"
    it "handles inline bracket" do
      assertRoundTrip "| ABC def g3 | [L: 1/8] A3 A3 |\x0D\n"
    it "handles inline bracket 1" do
      assertRoundTrip "| ABC def g3 |[L: 1/8] A3 A3 |\x0D\n"
    it "handles new key" do
      assertRoundTrip "| ABc |\x0D\nK: F#Major\x0D\n| def |\x0D\n"
    it "handles new tempo" do
      assertRoundTrip "| ABc |\x0D\nM: 3/4\x0D\n| def |\x0D\n"
    it "handles new unit length" do
      assertRoundTrip "| ABc |\x0D\nL: 1/16\x0D\n| def |\x0D\n"
    it "handles new part" do
      assertRoundTrip "| ABc |\x0D\nP: B\x0D\n| def |\x0D\n"
    it "handles continuation" do
      let
        text = "| ABc |\\\x0D\n| def |\x0D\n"
      assertRoundTrip text
      -- we now coalesce the lines after a continuation
      assertMusicLines text 1
    it "handles continuation with comment" do
      let
        text = "| ABc |\\ ignored comment\x0D\n| def |\x0D\n"
      assertRoundTrip text
      -- we now coalesce the lines after a continuation
      assertMusicLines text 1
    it "handles empty bar" do
      assertRoundTrip "| ABc | | def |\x0D\n"
    it "handles inline key" do
      assertRoundTrip "| ABC def g3 | [K: AMajor] g3 a3 |\x0D\n"
    it "handles inline comment" do
      assertRoundTrip "| ABC z2 def z/ \x0D\n% this is a comment\x0D\n| ABC z2 def z/ |\x0D\n"
    it "handles outmoded line-break" do
      assertParses "| ABc | def |!\x0D\n| ABC|\x0D\n"
    it "handles workaround missing line feed line terminator" do
      assertParses "| ABc | def |\x0D| ABC|\x0D\n"

-- | the purescript version handles parsing differently from the elm version 
-- | when two different productions have the same initial lexeme.
-- | The purescript parser requires you to use 'try' to resolve the parse
ambiguitySpec :: Spec Unit
ambiguitySpec =
  describe "ambiguity" do
    it "handles ambiguous A" do
      assertRoundTrip "K: GMajor\r\nA\r\n"
    it "handles ambiguous a" do
      assertRoundTrip "K: GMajor\r\na\r\n"

badInputSpec :: Spec Unit
badInputSpec =
  describe "bad input" do
    it "handles bad chars 1" do
      assertParseError "| ABC z2 def z/ |\x0D\n| foo bar |\x0D\n"
    it "handles bad chars 2" do
      assertParseError "| foo bar |\x0D\n| ABC z2 def z/ |\x0D\n"
    it "handles bracket in inline header" do
      assertParseError "| ABC |\x0D\nr: this is a remark [part 1]\x0D\n"
    it "handles too few notes in short form triplet" do
      assertParseError "| ABC (3DE | GGG |\x0D\n"
    it "handles too few notes in long form triplet" do
      assertParseError "| ABC (3:2:3DE | GGG |\x0D\n"

keySigSpec :: Spec Unit
keySigSpec =
  describe "key signature parser" do
    it "recognizes G" do
      assertKeySigParses "G"
    it "recognizes C# Major" do
      assertKeySigParses "C# Major"
    it "recognizes G Mixolydian" do
      assertKeySigParses "G Mixolydian"
    it "recognizes A Locrian" do
      assertKeySigParses "A Locrian"
    it "recognizes Bb Minor" do
      assertKeySigParses "Bb Minor"

-- these ABC samples are already in canonical format which should allow round-tripping to work
-- because of the exact string matching algorithm

keyWithAccidental =
  "K: EMinor ^c\x0D\n| ABC |\x0D\n"

keyCMajor =
  "K: CMajor\x0D\n| ABC |\x0D\n"

keyADorian =
  "K: ADorian\x0D\n| ABC |\x0D\n"

halfNoteCanonical =
  "| B/ |\r\n"

quarterNoteCanonical =
  "| D1/4 |\r\n"

eighthNoteCanonical =
  "| D1/8 |\r\n"

repeat =
  "|: ABCD EFGa ||1 D4 C4 :||2 c8 |\r\n"

repeat3 =
  "|: ABCD EFGa :|: c8 |\x0D\n"

standardTempo =
  "Q: 1/4=120\x0D\n| ABC |\x0D\n"

suffixedTempo =
  "Q: 1/4=70 \"lento\"\x0D\n| ABC |\x0D\n"
