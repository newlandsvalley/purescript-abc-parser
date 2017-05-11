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
import Data.List (List, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, runRational)
import Data.Ratio (Ratio(..))
import Data.Tuple (Tuple(..))
import Data.String (trim, toLower, singleton, length, take) as Str
import Data.Foldable (foldr)
import Data.Either (Either)
import Data.Newtype (unwrap)

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

        eq =
            if (length t.noteLengths == 0) then
                ""
            else
                "="
    in
        ratlist t.noteLengths
            <> eq
            <> show t.bpm
            <> text

{-  ps 0.10.7
rational :: Rational -> String
rational (Rational (Ratio n d)) =
    show n <> "/" <> show d
-}


showRatio :: Ratio Int -> String
showRatio (Ratio n d) =
    (show n) <> "/" <> (show d)

ratlist :: List Rational -> String
ratlist rs =
    let
        f r acc =
            (showRatio (runRational r)) <> " " <> acc
    in
      Str.trim $ foldr f "" rs
    {-
        List.foldr f "" rs
            |> trimRight
    -}

meter :: Maybe MeterSignature -> String
meter ms =
    case ms of
        Nothing ->
            "none"

        Just (Tuple n d) ->
            show n <> "/" <> show d

duration :: Rational -> String
duration r =
  duration' (runRational r)

duration' :: Ratio Int -> String
duration' (Ratio 1 1 )  = ""
duration' (Ratio 1 2 )  = "/"
duration' (Ratio n 1 )  = show n
duration' r = showRatio r

key :: KeySignature -> String
key k =
  show k.pitchClass <> (keySignatureAccidental k.accidental) <> show k.mode

keyAccidental :: KeyAccidental -> String
keyAccidental nka =
  show (unwrap nka).accidental <> Str.toLower (show (unwrap nka).pitchClass)

keyAccidentals :: List KeyAccidental -> String
keyAccidentals =
    concatenate <<< map (\a -> " " <> keyAccidental a)

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
        acc =
            fromMaybe "" $ map show a.accidental

        tie =
            case a.tied of
                true ->
                    "-"

                _ ->
                    ""
    in
        acc
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

notes :: List AbcNote -> String
notes ns =
    let
        f a acc =
            (abcNote a) <> acc
    in
        foldr f "" ns

rest :: NoteDuration -> String
rest n =
    "z" <> (duration n)

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
            rest r

        Tuplet tup ns ->
            tuplet tup <> notes ns

        Decoration s ->
            decorate s

        GraceNote isAcciaccatura ns ->
            "{" <> notes ns <> "}"

        Slur c ->
            Str.singleton c

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
