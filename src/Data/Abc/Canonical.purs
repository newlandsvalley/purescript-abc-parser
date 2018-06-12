-- | A canonical representation of an ABC tune as a string.
module Data.Abc.Canonical
        ( fromTune
        , fromEither
        , abcNote
        , abcChord
        , tuplet
        , keySignatureAccidental
        ) where

import Prelude (map, show, ($), (<>), (<<<), (+), (-), (<=), (>), (==), (||))
import Data.Abc
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, numerator, denominator)
import Data.Tuple (Tuple(..))
import Data.String (trim, toLower, length, take) as Str
import Data.String.CodePoints (codePointFromChar, singleton)
import Data.Foldable (foldr)
import Data.Either (Either(..))

-- | Module for converting an ABC Tune parse tree to a canonical ABC string

enquote :: String -> String
enquote s =
    "\"" <> s <> "\""

bar :: Bar -> String
bar b =
    let
        it =
            fromMaybe "" (map show b.iteration)
        lines = show b.thickness
    in
        case b.repeat of
            Nothing ->
                lines <> it

            Just Begin ->
                lines <> ":"

            Just End ->
                ":" <> lines <> it

            Just BeginAndEnd ->
                ":" <> lines <> ":"

keySignatureAccidental :: Accidental -> String
keySignatureAccidental a =
  case a of
    Sharp ->
      "#"
    Flat ->
      "b"
    _ ->
      ""

-- | Pretty-print a tuplet.
tuplet :: TupletSignature -> String
tuplet { p: 2, q: 3, r: 2 } = "(2"
tuplet { p: 3, q: 2, r: 3 } = "(3"
tuplet { p: 4, q: 3, r: 4 } = "(4"
tuplet { p: p, q: q, r: r } =
    "("
        <> (show p)
        <> ":"
        <> (show q)
        <> ":"
        <> (show r)

tempo :: TempoSignature -> String
tempo t =
  let
    text =
      fromMaybe "" (map (\s -> " " <> (enquote s)) t.marking)
  in
    ratlist t.noteLengths
      <> "="
      <> show t.bpm
      <> text

showRatio :: Rational -> String
showRatio r =
  (show $ numerator r) <> "/" <> (show $ denominator r)

ratlist :: NonEmptyList Rational -> String
ratlist rs =
    let
        f r acc =
          (showRatio r) <> " " <> acc
    in
      Str.trim $ foldr f "" rs

meter :: Maybe MeterSignature -> String
meter ms =
    case ms of
        Nothing ->
            "none"

        Just (Tuple n d) ->
            show n <> "/" <> show d

duration :: Rational -> String
duration r =
  case (Tuple (numerator r) (denominator r)) of
    Tuple 1 1 -> ""
    Tuple 1 2 -> "/"
    Tuple n 1 -> show n
    Tuple _ _ -> showRatio r

key :: KeySignature -> String
key k =
  show k.pitchClass <> (keySignatureAccidental k.accidental) <> show k.mode

keyAccidentals :: List Pitch -> String
keyAccidentals =
    concatenate <<< map (\ka -> " " <> (show ka))

octave :: Int -> String
octave i =
    let
        octaveAboveMiddleC =
            middlecOctave + 1
    in
        if ((i == middlecOctave) || (i == octaveAboveMiddleC)) then
            ""
        else if (i > octaveAboveMiddleC) then
            -- catChars $ replicate (i - octaveAboveMiddleC) '\''
            Str.take (i - octaveAboveMiddleC) "''''''''''"
        else
            Str.take (middlecOctave - i) ",,,,,,,,,,"

pitch :: Int -> PitchClass -> String
pitch octaveNumber p =
    if (octaveNumber <= middlecOctave) then
        show p
    else
        Str.toLower (show p)


-- | Pretty-print a note.
abcNote :: AbcNote -> String
abcNote a =
  let
    tie =
      case a.tied of
        true ->
          "-"
        _ ->
          ""
  in
    show a.accidental
      <> pitch a.octave a.pitchClass
      <> octave a.octave
      <> duration a.duration
      <> tie

-- | Pretty-print a chord.
abcChord :: AbcChord -> String
abcChord a =
    "["
        <> (notes a.notes)
        <> "]"
        <> duration a.duration

notes :: NonEmptyList AbcNote -> String
notes ns =
    let
        f a acc =
            (abcNote a) <> acc
    in
        foldr f "" ns

restsOrNotes :: NonEmptyList RestOrNote -> String
restsOrNotes rns =
  let
    f :: RestOrNote -> String -> String
    f rn acc =
      case rn of
        Left r ->
          (abcRest r) <> acc
        Right n ->
          (abcNote n) <> acc
  in
    foldr f "" rns

abcRest :: AbcRest -> String
abcRest r =
    "z" <> (duration r.duration)

decorate :: String -> String
decorate s =
    if (Str.length s == 1) then
        s
    else
        "!" <> s <> "!"

musics :: List Music -> String
musics ms =
    let
        f m acc =
            (music m) <> acc
    in
        foldr f "" ms

broken :: Broken -> String
broken b =
    case b of
        LeftArrow i ->
            Str.take i "<<<<<<<<<<"

        RightArrow i ->
            Str.take i ">>>>>>>>>>"

music :: Music -> String
music m =
    case m of
        Barline b ->
            bar b

        Note a ->
            abcNote a

        BrokenRhythmPair a1 b a2 ->
            abcNote a1 <> (broken b) <> abcNote a2

        Rest r ->
            abcRest r

        Tuplet tup rns ->
            tuplet tup <> restsOrNotes rns

        Decoration s ->
            decorate s

        GraceNote isAcciaccatura ns ->
            "{" <> notes ns <> "}"

        Slur c ->
            singleton $ codePointFromChar c

        Annotation placement s ->
            show placement <> ":" <> s

        ChordSymbol s ->
            enquote s

        Chord a ->
            abcChord a

        Inline h ->
            "[" <> header h <> "]"

        Spacer i ->
            " "

        Ignore ->
            ""

        Continuation ->
            "\\"

header :: Header -> String
header h =
    case h of
        Area s ->
            "A: " <> s

        Book s ->
            "B: " <> s

        Composer s ->
            "C: " <> s

        Discography s ->
            "D: " <> s

        FileUrl s ->
            "F: " <> s

        Group s ->
            "G: " <> s

        History s ->
            "H: " <> s

        Instruction s ->
            "I: " <> s

        Key mks ->
            "K: " <> (key mks.keySignature) <> (keyAccidentals mks.modifications)

        UnitNoteLength d ->
            "L: " <> (duration d)

        Meter m ->
            "M: " <> (meter m)

        Macro s ->
            "m: " <> s

        Notes s ->
            "N: " <> s

        Origin s ->
            "O: " <> s

        Parts s ->
            "P: " <> s

        Rhythm s ->
            "R: " <> s

        Remark s ->
            "r: " <> s

        Source s ->
            "S: " <> s

        Title s ->
            "T: " <> s

        Tempo t ->
            "Q: " <> (tempo t)

        UserDefined s ->
            "U: " <> s

        Voice s ->
            "V: " <> s

        WordsAfter s ->
            "W: " <> s

        WordsAligned s ->
            "w: " <> s

        ReferenceNumber i ->
            "X: " <> (show i)

        Transcription s ->
            "Z: " <> s

        FieldContinuation s ->
            "+: " <> s

        Comment s ->
            "%" <> s

        _ ->
            ""


tuneHeaders :: List Header -> String
tuneHeaders hs =
    let
        f h acc =
            (header h) <> "\x0D\n" <> acc
    in
        foldr f "" hs

bodyPart :: BodyPart -> String
bodyPart bp =
    case bp of
        Score ml ->
            musics ml

        BodyInfo h ->
            header h

continuation :: Boolean -> String
continuation c =
    if c then
        "\\"
    else
        ""

tuneBody :: TuneBody -> String
tuneBody b =
-- import Data.Either (Either(..))
    let
        f bp acc =
            (bodyPart bp) <> "\x0D\n" <> acc
    in
        foldr f "" b

concatenate :: List String -> String
concatenate = foldr (<>) ""

-- Main Exported Functions

-- | Translate an ABC Tune parse tree to a canonical ABC String.
fromTune :: AbcTune -> String
fromTune abcTune =
    tuneHeaders abcTune.headers <> tuneBody abcTune.body

-- | Translate a parse Result containing an ABC Tune parse tree to a Result containing a canonical ABC String.
fromEither :: Either String AbcTune -> Either String String
fromEither r =
    map fromTune r
