module Abc.Canonical
        ( fromTune
        , fromResult
        , abcNote
        , abcChord
        , tuplet
        ) where



import Prelude
import Abc.ParseTree
import Data.List (List, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational(..), (%))
import Data.Ratio (Ratio(..))
import Data.Tuple (Tuple(..))
import Data.String (trim, toLower, singleton, length, take) as Str
import Data.Foldable (foldr)
import Data.Either (Either)

{-| Module for converting an ABC Tune parse tree to a canonical ABC string,

-}


enquote :: String -> String
enquote s =
    "\"" <> s <> "\""

{-
mode :: Mode -> String
mode m =
    case m of
        Major ->
            "major"

        Minor ->
            "minor"

        Ionian ->
            "ionian"

        Dorian ->
            "dorian"

        Phrygian ->
            "phrygian"

        Lydian ->
            "lydian"

        Mixolydian ->
            "mixolydian"

        Aeolian ->
            "aeolian"

        Locrian ->
            "locrian"
-}

bar :: Bar -> String
bar b =
    let
        it =
            fromMaybe "" (map show b.iteration)

        lines = show b.thickness

        {-
            case b.thickness of
                ThickThin ->
                    "[|"

                ThinThick ->
                    "|]"

                ThinThin ->
                    "||"

                _ ->
                    "|"
          -}
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

{-
accidental :: Accidental -> String
accidental a =
    case a of
        Sharp ->
            "^"

        Flat ->
            "_"

        DoubleSharp ->
            "^^"

        DoubleFlat ->
            "__"

        Natural ->
            "="
            -}


headerAccidental :: Accidental -> String
headerAccidental a =
    case a of
        Sharp ->
            "#"

        Flat ->
            "b"

        _ ->
            ""


{-| Pretty-print a tuplet.-}

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

{-}
tuplet t =
    let
        TupletSignature { p: p, q: q, r: r } =
            t
    in
        case t of
            ( 2, 3, 2 ) ->
                "(2"

            ( 3, 2, 3 ) ->
                "(3"

            ( 4, 3, 4 ) ->
                "(4"

            _ ->
                "("
                    ++ (toString p)
                    ++ ":"
                    ++ (toString q)
                    ++ ":"
                    ++ (toString r)
-}


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


rational :: Rational -> String
rational (Rational (Ratio n d)) =
    show n <> "/" <> show d


ratlist :: List Rational -> String
ratlist rs =
    let
        f r acc =
            (rational r) <> " " <> acc
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


duration :: NoteDuration -> String
duration (Rational (Ratio 1 1 ))  = ""
duration (Rational (Ratio 2 1 ))  = "/"
duration (Rational (Ratio n 1 ))  = show n
duration r = rational r

key :: KeySignature -> String
key k =
    let
        acc =
            fromMaybe "" $ map headerAccidental k.accidental
    in
        show k.pitchClass <> acc <> show k.mode


keyAccidental :: KeyAccidental -> String
keyAccidental (Tuple pc acc) =
        show acc <> Str.toLower (show pc)


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


{-| Pretty-print a note.
-}
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


{-| Pretty-print a chord.
-}
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

        Key (Tuple k kacc) ->
            "K: " <> (key k) <> (keyAccidentals kacc)

{-
        Key ( k, kacc ) ->
            "K: " ++ (key k) ++ (keyAccidentals kacc)
-}

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


{-| Translate an ABC Tune parse tree to a canonical ABC String.
-}
fromTune :: AbcTune -> String
fromTune abcTune =
    tuneHeaders abcTune.headers <> tuneBody abcTune.body


{-| Translate a parse Result containing an ABC Tune parse tree to a Result containing a canonical ABC String.
-}
fromResult :: Either String AbcTune -> Either String String
fromResult r =
    map fromTune r
