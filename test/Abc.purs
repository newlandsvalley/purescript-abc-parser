module Test.Abc (abcSuite) where

import Prelude
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.List (length)
import Data.Abc.Parser (parse, parseKeySignature)
import Data.Abc.Canonical (fromTune)

import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert

assertRoundTrip :: String -> Test
assertRoundTrip s =
  assertCanonical s s

assertCanonical :: String -> String -> Test
assertCanonical s canonical =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right tune ->
        Assert.equal canonical (fromTune tune)

      Left err ->
        failure ("parse failed: " <> (show err))

assertParses :: String -> Test
assertParses s =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right res ->
        success

      Left err ->
        failure ("parse failed: " <> (show err))

assertParseError :: String -> Test
assertParseError s =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right res ->
        failure "parses when it shouldn't"

      Left err ->
        success

assertKeySigParses :: String -> Test
assertKeySigParses s =
  let
    parseResult =
      parseKeySignature s
  in
    case parseResult of
      Right res ->
        success

      Left err ->
        failure ("parse failed: " <> (show err))

assertMusicLines :: String -> Int -> Test
assertMusicLines s target =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right tune ->
        Assert.equal target (length tune.body)

      Left err ->
        failure ("parse failed: " <> (show err))


abcSuite :: Free TestF Unit
abcSuite = do
   headerSuite
   noteSuite
   barSuite
   phrasingSuite
   structureSuite
   ambiguitySuite
   badInputSuite
   keySigSuite

headerSuite :: Free TestF Unit
headerSuite =
  suite "headers" do
    test "area" do
      assertRoundTrip "A: London\x0D\n| ABC |\x0D\n"
    test "book" do
      assertRoundTrip "B: Richie Robinson\x0D\n| ABC |\x0D\n"
    test "composer" do
      assertRoundTrip "C: Bys-Kalle\x0D\n| ABC |\x0D\n"
    test "discography" do
      assertRoundTrip "D: 2 Brudetstykke\x0D\n| ABC |\x0D\n"
    test "file URL" do
      assertRoundTrip "F: http\\\\tradtunedb.org.uk\x0D\n| ABC |\x0D\n"
    test "group" do
      assertRoundTrip "G: Swåp\x0D\n| ABC |\x0D\n"
    test "history" do
      assertRoundTrip "H: Learned from AnnbjØrg Lien\x0D\n| ABC |\x0D\n"
    test "instruction" do
      assertRoundTrip "I: abc-charset UTF-8\x0D\n| ABC |\x0D\n"
    test "key" do
      assertRoundTrip keyADorian
    test "key spaced" do
      assertCanonical "K: A Dorian\x0D\n| ABC |\x0D\n" keyADorian
    test "key with accidental" do
      assertRoundTrip "K: AMinor ^f\x0D\n| ABC |\x0D\n"
    test "key with unspaced accidental" do
      assertCanonical "K: EMinor^c\x0D\n| ABC |\x0D\n" keyWithAccidental
    test "simple key" do
      assertCanonical "K: C\x0D\n| ABC |\x0D\n" keyCMajor
    test "key trailing space" do
      assertCanonical "K: CMajor \x0D\n| ABC |\x0D\n" keyCMajor
    test "note length" do
        assertRoundTrip "L: 1/8\x0D\n| ABC |\x0D\n"
    test "meter" do
        assertRoundTrip "M: 3/4\x0D\n| ABC |\x0D\n"
    test "no meter" do
      assertRoundTrip "M: none\x0D\n| ABC |\x0D\n"
    test "macro" do
       assertRoundTrip "m: ~g2 = {a}g{f}g\x0D\n| ABC |\x0D\n"
    test "notes" do
       assertRoundTrip "N: from recording made at Tideswell\x0D\n| ABC |\x0D\n"
    test "origin" do
      assertRoundTrip "O: Skåne\x0D\n| ABC |\x0D\n"
    test "parts" do
      assertRoundTrip "P: ((AB)3.(CD)3)2\x0D\n| ABC |\x0D\n"
    test "tempo" do
      assertRoundTrip standardTempo
    test "suffixed tempo" do
      assertRoundTrip suffixedTempo
    test "prefixed tempo" do
      assertCanonical "Q: \"lento\" 1/4=70\x0D\n| ABC |\x0D\n" suffixedTempo
    test "degenerate tempo" do
      assertCanonical "Q: 120\x0D\n| ABC |\x0D\n" standardTempo
    test "tempo trailing space" do
      assertCanonical "Q: 1/4=120  \x0D\n| ABC |\x0D\n" standardTempo
    test "multi-beat tempo" do
      assertRoundTrip "Q: 1/4 3/8 1/4 3/8=40\x0D\n| ABC |\x0D\n"
    test "remark" do
      assertRoundTrip "r: this is a remark\x0D\n| ABC |\x0D\n"
    test "rhythm" do
      assertRoundTrip "R: Polska\x0D\n| ABC |\x0D\n"
    test "source" do
      assertRoundTrip "S: Christine Dyer\x0D\n| ABC |\x0D\n"
    test "title" do
      assertRoundTrip "T: Engelska efter Albert Augustsson\x0D\n| ABC |\x0D\n"
    test "user-defined" do
      assertRoundTrip "U: some comment\x0D\n| ABC |\x0D\n"
    test "simple voice" do
      assertRoundTrip "V: T1\x0D\n| ABC |\x0D\n"
    test "voice" do
      assertRoundTrip "V: T1 clef=treble-8+8 name=\"Tenore I\" snm=\"T.I\"\x0D\n| ABC |\x0D\n"
    test "words after" do
      assertRoundTrip "W: doh re mi fa \x0D\n| ABC |\x0D\n"
    -- the words aligned header only appears inline
    test "words aligned" do
      assertRoundTrip "| ABC |\x0D\nw: doh re mi fa \x0D\n| ABC |\x0D\n"
    test "reference" do
      assertRoundTrip "X: 125\x0D\n| ABC |\x0D\n"
    test "transcriber" do
      assertRoundTrip "Z: John Watson\x0D\n| ABC |\x0D\n"
    test "field continuation" do
      assertRoundTrip "R: Polska\x0D\n+: in triplet time\x0D\n| ABC |\x0D\n"
    test "comment" do
      assertRoundTrip "%%TBL:{\"version\":\"beta\",\"type\":\"tune\",\"id\":\"10294\"}\x0D\n| ABC |\x0D\n"
    test "unsupported header" do
      assertParses "j: custom header\x0D\n| ABC |\x0D\n"
    test "bracket in header" do
      assertRoundTrip "r: this is a remark [part 1]\x0D\n| ABC |\x0D\n"

noteSuite :: Free TestF Unit
noteSuite =
  suite "note" do
    test "single duration" do
       assertRoundTrip "| A |\r\n"
    test "doubly implied half duration" do
       assertRoundTrip "| B/ |\r\n"
    test "implied half duration" do
       assertCanonical "| B/2 |\r\n" halfNoteCanonical
    test "explicit half duration" do
       assertCanonical "| B1/2 |\r\n" halfNoteCanonical
    test "quarter duration" do
       assertCanonical "| D// |\r\n" quarterNoteCanonical
    test "eighth duration" do
       assertCanonical "| D/// |\r\n" eighthNoteCanonical
    test "double duration" do
       assertRoundTrip "| a2 |\r\n"
    test "broken rhythm" do
       assertRoundTrip "| A>B C>>D a<b c<<d |\x0D\n"
    test "broken rhythm spaced" do
       assertCanonical "| A> B |\x0D\n" "| A>B |\x0D\n"
    test "octave" do
       assertRoundTrip "| A,B,,C z2 d'e''f z/ |\x0D\n"
    test "tie" do
       assertRoundTrip "| A4- A2 |\x0D\n"
    test "complex tie" do
       assertRoundTrip "| fg-ga ab-bc|\x0D\n"
    test "triplet" do
       assertRoundTrip "| (3efg |\r\n"
    test "spaced triplet" do
       assertCanonical "| (3 abc def |\x0D\n" "| (3abc def |\x0D\n"
    test "triplet with rest" do
       assertRoundTrip "| (3zfg |\r\n"
    test "grace note" do
       assertRoundTrip "| {d^f}GA |\x0D\n"
    test "grace note in tuplet" do
       assertRoundTrip "| (3c{d}fg A |\x0D\n"
    test "grace note in broken rhythm pair" do
       assertRoundTrip "| A>{f}B C>>{ef}D |\x0D\n"
    test "double sharp" do
       assertRoundTrip "| ^^C2 |\r\n"
    test "sharp" do
       assertRoundTrip "| ^C/ |\r\n"
    test "double flat" do
       assertRoundTrip "| __C |\r\n"
    test "flat" do
       assertRoundTrip "| _C3/2 |\r\n"
    test "natural" do
       assertRoundTrip "| =C3/2 |\r\n"
    test "chord symbol" do
       assertRoundTrip "| \"Em\" EG \"Am\" AC |\x0D\n"
    test "chords" do
       assertRoundTrip "| [de^f]g [cda]b |\x0D\n"
    test "chord duration" do
       assertRoundTrip "| [cda]4 |\x0D\n"


barSuite :: Free TestF Unit
barSuite =
  suite "bar lines" do
    test "no bars" do
      assertRoundTrip "A B C\r\n"
    test "no starting or terminating line" do
      assertRoundTrip "A B C | D E F\r\n"
    test "no terminating line" do
      assertRoundTrip "| A B C | D E F\r\n"
    test "no bars stave 2" do
      assertRoundTrip "| A B C |\r\n D E F\r\n"
    {-}
    test "from failing transposition test" do
      assertRoundTrip "| G3A B6 Ac |\r\n B2AG ^FGA^F D4\r\n"
    -}
    test "repeat" do
      assertRoundTrip "|: A :|\r\n"
    test "bracket line" do
      assertRoundTrip "[| A |]\r\n"
    test "double colon" do
      assertCanonical "||: A :: c :||\r\n" "||: A :|: c :||\r\n"
    test "alternate endings" do
      assertRoundTrip "| A |1 B :|2 c||\r\n"
    test "repeat 0" do
      assertRoundTrip  "|: ABCD EFGa |1 D4 C4 :|2 c8 |\x0D\n"
    test "repeat 1" do
      assertCanonical  "|: ABCD EFGa |[1 D4 C4 :|[2 c8 |\x0D\n" repeat
    test "repeat 1a" do
      assertRoundTrip  "|: ABCD EFGa [|1 D4 C4 :[|2 c8 |]\x0D\n"
    test "repeat 2" do
      assertRoundTrip  "|: ABCD EFGa [|1 D4 C4 :[|2 c8 |]\x0D\n"
    test "repeat 3" do
      assertRoundTrip repeat3
    test "repeat 3a" do
      assertCanonical  "|: ABCD EFGa :|: c8 |\x0D\n" repeat3
    test "repeat 3b" do
      assertRoundTrip  "|: ABCD EFGa :||: c8 |\x0D\n"
    test "repeat 4" do
      assertRoundTrip  "[|2 ABCD EFGa |]: c8 |\x0D\n"
    test "repeat 5" do
      assertRoundTrip  "|: ABCD EFGa :|] c8 |\x0D\n"
    test "repeat 6" do
      assertRoundTrip  "[|2 ABCD EFGa ||: c8 |\x0D\n"
    test "repeat 7" do
      assertRoundTrip  "| ABCD EFGa :|| c8 |\x0D\n"
    test "degenerate repeat 1" do
      assertCanonical  "[1 ABCD |\x0D\n" "|1 ABCD |\x0D\n"
    test "degenerate repeat 2" do
      assertCanonical  "| [1 ABCD |\x0D\n" "| |1 ABCD |\x0D\n"

phrasingSuite :: Free TestF Unit
phrasingSuite  =
  suite "phrasing" do
    test "slur" do
      assertRoundTrip  "| (de^f) (cda) |\x0D\n"
    test "articulation" do
      assertRoundTrip  "(vA2 | !fz! Ld2).d.f .e.d.c.B A2(A2 | d2).d.f .e.d.c.B A2A2 |\x0D\n"
    test "annotation" do
      assertRoundTrip  "| \"<(\" \">)\" EG |\x0D\n"
    test "decorated note" do
      assertRoundTrip  "| !uppermordent! !trill! C |\x0D\n"
    test "decorated space" do
      assertRoundTrip  "| ABc !coda! y |\x0D\n"

structureSuite :: Free TestF Unit
structureSuite  =
  suite "structure" do
    test "ignore" do
      assertParses  "| ABC# z2 @def z/ |\x0D\n"
    test "typeset space" do
      assertParses  "| ABC yz2 defyz/ |\x0D\n"
    test "backtick" do
      assertParses "| A``B``C |\x0D\n"
    test "inline" do
      assertRoundTrip "| ABC z2 def z/ \x0D\nQ: 1/4=120\x0D\n| ABC z2 def z/ |\x0D\n"
    test "inline voice reference" do
      assertRoundTrip "[V: T1]| ABC |\x0D\n"
    test "inline bracket" do
      assertRoundTrip "| ABC def g3 | [L: 1/8] A3 A3 |\x0D\n"
    test "inline bracket 1" do
      assertRoundTrip "| ABC def g3 |[L: 1/8] A3 A3 |\x0D\n"
    test "new key" do
      assertRoundTrip "| ABc |\x0D\nK: F#Major\x0D\n| def |\x0D\n"
    test "new tempo" do
      assertRoundTrip "| ABc |\x0D\nM: 3/4\x0D\n| def |\x0D\n"
    test "new unit length" do
      assertRoundTrip "| ABc |\x0D\nL: 1/16\x0D\n| def |\x0D\n"
    test "new part" do
      assertRoundTrip "| ABc |\x0D\nP: B\x0D\n| def |\x0D\n"
    test "continuation" do
      let
        text = "| ABc |\\\x0D\n| def |\x0D\n"
      assertRoundTrip text
      -- we now coalesce the lines after a continuation
      assertMusicLines text 1
    test "continuation with comment" do
      let
        text = "| ABc |\\ ignored comment\x0D\n| def |\x0D\n"
      assertRoundTrip text
      -- we now coalesce the lines after a continuation
      assertMusicLines text 1
    test "empty bar" do
      assertRoundTrip "| ABc | | def |\x0D\n"
    test "inline key" do
      assertRoundTrip "| ABC def g3 | [K: AMajor] g3 a3 |\x0D\n"
    test "inline comment" do
      assertRoundTrip "| ABC z2 def z/ \x0D\n%% this is a comment\x0D\n| ABC z2 def z/ |\x0D\n"

-- | the purescript version handles parsing differently from the elm version
-- | when two different productions have the same initial lexeme.
-- | The purescript parser requires you to use 'try' to resolve the parse
ambiguitySuite :: Free TestF Unit
ambiguitySuite =
  suite "ambiguity" do
    test "ambiguous A" do
      assertRoundTrip "K: GMajor\r\nA\r\n"
    test "ambiguous a" do
      assertRoundTrip "K: GMajor\r\na\r\n"


badInputSuite :: Free TestF Unit
badInputSuite =
  suite "bad input" do
    test "bad chars 1" do
      assertParseError "| ABC z2 def z/ |\x0D\n| foo bar |\x0D\n"
    test "bad chars 2" do
      assertParseError "| foo bar |\x0D\n| ABC z2 def z/ |\x0D\n"
    test "bracket in inline header" do
      assertParseError "| ABC |\x0D\nr: this is a remark [part 1]\x0D\n"

keySigSuite :: Free TestF Unit
keySigSuite =
  suite "key signature parser" do
    test "G" do
      assertKeySigParses "G"
    test "C# Major" do
      assertKeySigParses "C# Major"
    test "G Mixolydian" do
      assertKeySigParses "G Mixolydian"
    test "A Locrian" do
      assertKeySigParses "A Locrian"
    test "Bb Minor" do
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
