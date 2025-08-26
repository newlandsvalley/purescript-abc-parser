-- | A canonical representation of an ABC tune as a string.
module Data.Abc.Canonical
  ( fromTune
  , fromEither
  , abcNote
  , abcChord
  , tupletSignature
  , bars
  , keySignatureAccidental
  , normaliseKey
  ) where

import Prelude (map, show, ($), (<>), (<<<), (+), (-), (<=), (>), (==), (||))
import Data.Abc
import JS.BigInt (toString) as BigInt
import Data.List (List, foldMap)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Rational (Rational, numerator, denominator)
import Data.Tuple (Tuple(..))
import Data.Map (size, toUnfoldable)
import Data.Semigroup.Foldable (intercalateMap)
import Data.String (trim, toLower, length, take) as Str
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (repeat)
import Data.Foldable (foldr, intercalate)
import Data.Unfoldable (replicate)
import Data.Either (Either(..))

-- | Module for converting an ABC Tune parse tree to a canonical ABC string

keySignatureAccidental :: Accidental -> String
keySignatureAccidental a =
  case a of
    Sharp ->
      "#"
    Flat ->
      "b"
    _ ->
      ""

-- | Pretty-print a tuplet signature.
tupletSignature :: TupletSignature -> String
tupletSignature { p: 2, q: 3, r: 2 } = "(2"
tupletSignature { p: 3, q: 2, r: 3 } = "(3"
tupletSignature { p: 4, q: 3, r: 4 } = "(4"
tupletSignature { p: p, q: q, r: r } =
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
      fromMaybe "" (map (\s -> " " <> s) t.marking)
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

meter :: Maybe TimeSignature -> String
meter ms =
  case ms of
    Nothing ->
      "none"

    Just { numerator, denominator } ->
      show numerator <> "/" <> show denominator

-- we optimise durations in tune bodies to the most compact form
-- just use showRatio in headers
duration :: Rational -> String
duration r =
  case (BigInt.toString $ numerator r),  (BigInt.toString $ denominator r) of
    "1", "1" -> ""
    "1" ,"2" -> "/"
    n, "1" -> n
    _, _ -> showRatio r


-- | Normalise a key signature to a standard canonical format as a string
normaliseKey :: KeySignature -> String
normaliseKey k =
  show k.pitchClass <> (keySignatureAccidental k.accidental) <> show k.mode

keyAccidentals :: List Pitch -> String
keyAccidentals =
  concatenate <<< map (\ka -> " " <> (show ka))

-- amorphous properties of either the Key or Voice header
amorphousProperties :: AmorphousProperties -> String
amorphousProperties properties =
  if (size properties == 0) then ""
  else
    let
      kvs = toUnfoldable properties

      strs :: Array String
      strs = map (\(Tuple k v) -> k <> "=" <> v) kvs
    in
      " " <> intercalate " " strs

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

-- | Pretty-print a note which may be prefaced by grace notes and/or decorations.
graceableNote :: GraceableNote -> String
graceableNote gn =
  (maybeGrace gn.maybeGrace)
    <> leftSlurs gn.leftSlurs
    <> decorate gn.decorations
    <> abcNote (gn.abcNote)
    <> rightSlurs gn.rightSlurs

leftSlurs :: Int -> String
leftSlurs n =
  fromCharArray $ replicate n '('

rightSlurs :: Int -> String
rightSlurs n =
  fromCharArray $ replicate n ')'

-- | pretty print an optional grace note
maybeGrace :: Maybe Grace -> String
maybeGrace mGrace =
  fromMaybe "" $ map grace mGrace

grace :: Grace -> String
grace g =
  "{" <> notes g.notes <> "}"

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
abcChord c =
  leftSlurs c.leftSlurs
    <> decorate c.decorations
    <> "["
    <> (notes c.notes)
    <> "]"
    <> duration c.duration
    <> rightSlurs c.rightSlurs

notes :: NonEmptyList AbcNote -> String
notes ns =
  let
    f a acc =
      (abcNote a) <> acc
  in
    foldr f "" ns

-- generate a single free-standing rest or note (i.e. not part of a tuplet)
singleRestOrNote :: RestOrNote -> String
singleRestOrNote rn =
  restOrNote rn ""

-- a rest or note whih may be part of a sequence if the accumulator is used
restOrNote :: RestOrNote -> String -> String
restOrNote rn acc =
  case rn of
    Left r ->
      (abcRest r) <> acc
    Right n ->
      (graceableNote n) <> acc

restsOrNotes :: NonEmptyList RestOrNote -> String
restsOrNotes rns =
    foldr restOrNote "" rns

abcRest :: AbcRest -> String
abcRest r =
  "z" <> (duration r.duration)

decorate :: List String -> String
decorate ds =
  foldMap decorate1 ds

decorate1 :: String -> String
decorate1 s =
  if (Str.length s == 1) then
    s
  else
    "!" <> s <> "! "

bars :: List Bar -> String
bars bs =
  let
    f b acc =
      (bar b) <> acc
  in
    foldr f "" bs

bar :: Bar -> String
bar b =
  let
    f m acc =
      (music m) <> acc
  in
    decorate b.decorations
      <> barLine b.startLine
      <>
        foldr f "" b.music

barLine :: BarLine -> String
barLine b =
  let
    lines = show b.thickness
    endColons = fromMaybe "" $ repeat b.endRepeats ":"
    startColons = fromMaybe "" $ repeat b.startRepeats ":"
    iteration = fromMaybe "" $ map voltas b.iteration
  in
    endColons <> lines <> startColons <> iteration


voltas :: NonEmptyList Volta -> String
voltas vs =
  intercalateMap "," show vs

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

    Note gn ->
      graceableNote gn

    BrokenRhythmPair g1 b g2 ->
      (singleRestOrNote g1) <> (broken b) <> (singleRestOrNote g2) 

    Rest r ->
      abcRest r

    Tuplet t {- mGrace bracks tup rns -} ->
      (maybeGrace t.maybeGrace)
        <> leftSlurs t.leftSlurs
        <> tupletSignature t.signature
        <> restsOrNotes t.restsOrNotes
        
    DecoratedSpace decorations ->
      (decorate decorations) <> "y"

    Annotation placement s ->
      "\"" <> show placement <> s <> "\""

    ChordSymbol symbol ->
      "\"" <> symbol.name <> "\""

    Chord a ->
      abcChord a

    Inline h ->
      "[" <> header h <> "]"

    Spacer _ ->
      " "

    Ignore ->
      ""

    Continuation comment ->
      ("\\" <> comment <> "\r\n")

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
      "K: " <> (normaliseKey mks.keySignature) <> (keyAccidentals mks.modifications)
        <> amorphousProperties mks.properties

    UnitNoteLength d ->
      "L: " <> (showRatio d)

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

    Voice voiceDescription ->
      "V: " <> voiceDescription.id
        <> amorphousProperties voiceDescription.properties

    WordsAfter s ->
      "W: " <> s

    WordsAligned s ->
      "w: " <> s

    ReferenceNumber mi ->
      "X: " <> (maybe "" show mi)

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
    Score bs ->
      bars bs

    BodyInfo h ->
      header h

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
