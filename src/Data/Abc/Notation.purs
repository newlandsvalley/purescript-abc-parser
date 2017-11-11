module Data.Abc.Notation
        ( NoteTime
        , DiatonicScale
        , HeaderMap
        , notesInChromaticScale
        , pitchNumbers
        , pitchNumber
        , diatonicScale
        , inScale
        , keySet
        , modifiedKeySet
        , getKeySet
        , inKeySet
        , getHeader
        , getKeySig
        , getMeter
        , getTempoSig
        , getTitle
        , getUnitNoteLength
        , isCOrSharpKey
        , dotFactor
        , transposeKeySignatureBy
        , normaliseModalKey
        ) where


import Data.Abc

import Data.List (List(..), (:), elem, elemIndex, foldr, filter, index, length, null, reverse, slice)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, (%))
import Data.Tuple (Tuple(..))
import Prelude (map, mod, negate, show, ($), (+), (-), (/=), (<), (<>), (==), (||))

-- | A diatonic scale presented as a list of notes in the scale.
type DiatonicScale =
    List Pitch

-- | A chromatic (12-note) scale -}
type ChromaticScale =
    List Pitch

type Intervals =
    List Int

-- | The time taken when a note is played before the next note.
type NoteTime =
    Number

-- | A representation of the ABC headers as a Map, taking the first definition
-- | of any header if multiple definitions are present in the ABC.
type HeaderMap =
    Map Char Header

-- EXPORTED FUNCTIONS

-- | the number of notes in a chromatic scale (12)
notesInChromaticScale :: Int
notesInChromaticScale =
  12

-- | a relationship between a Pitch and a note number
-- | i.e. C is 0, C Sharp is 1 B is 11 etc.
pitchNumbers :: List ( Tuple Pitch Int )
pitchNumbers =
  ( Tuple (Pitch { pitchClass: C, accidental: Flat }) 11
  : Tuple (Pitch { pitchClass: C, accidental: Natural }) 0
  : Tuple (Pitch { pitchClass: C, accidental: Implicit }) 0
  : Tuple (Pitch { pitchClass: C, accidental: Sharp }) 1
  : Tuple (Pitch { pitchClass: C, accidental: DoubleSharp }) 2
  : Tuple (Pitch { pitchClass: D, accidental: DoubleFlat }) 0
  : Tuple (Pitch { pitchClass: D, accidental: Flat }) 1
  : Tuple (Pitch { pitchClass: D, accidental: Natural }) 2
  : Tuple (Pitch { pitchClass: D, accidental: Implicit }) 2
  : Tuple (Pitch { pitchClass: D, accidental: Sharp }) 3
  : Tuple (Pitch { pitchClass: D, accidental: DoubleSharp }) 4
  : Tuple (Pitch { pitchClass: E, accidental: DoubleFlat }) 2
  : Tuple (Pitch { pitchClass: E, accidental: Flat }) 3
  : Tuple (Pitch { pitchClass: E, accidental: Natural }) 4
  : Tuple (Pitch { pitchClass: E, accidental: Implicit }) 4
  : Tuple (Pitch { pitchClass: E, accidental: Sharp }) 5
  : Tuple (Pitch { pitchClass: E, accidental: DoubleSharp }) 6
  : Tuple (Pitch { pitchClass: F, accidental: Flat }) 4
  : Tuple (Pitch { pitchClass: F, accidental: Natural }) 5
  : Tuple (Pitch { pitchClass: F, accidental: Implicit }) 5
  : Tuple (Pitch { pitchClass: F, accidental: Sharp }) 6
  : Tuple (Pitch { pitchClass: F, accidental: DoubleSharp }) 7
  : Tuple (Pitch { pitchClass: G, accidental: DoubleFlat }) 5
  : Tuple (Pitch { pitchClass: G, accidental: Flat }) 6
  : Tuple (Pitch { pitchClass: G, accidental: Natural }) 7
  : Tuple (Pitch { pitchClass: G, accidental: Implicit }) 7
  : Tuple (Pitch { pitchClass: G, accidental: Sharp }) 8
  : Tuple (Pitch { pitchClass: G, accidental: DoubleSharp }) 9
  : Tuple (Pitch { pitchClass: A, accidental: DoubleFlat }) 7
  : Tuple (Pitch { pitchClass: A, accidental: Flat }) 8
  : Tuple (Pitch { pitchClass: A, accidental: Natural }) 9
  : Tuple (Pitch { pitchClass: A, accidental: Implicit }) 9
  : Tuple (Pitch { pitchClass: A, accidental: Sharp }) 10
  : Tuple (Pitch { pitchClass: A, accidental: DoubleSharp }) 11
  : Tuple (Pitch { pitchClass: B, accidental: DoubleFlat }) 9
  : Tuple (Pitch { pitchClass: B, accidental: Flat }) 10
  : Tuple (Pitch { pitchClass: B, accidental: Natural }) 11
  : Tuple (Pitch { pitchClass: B, accidental: Implicit }) 11
  : Tuple (Pitch { pitchClass: B, accidental: Sharp }) 0
  : Tuple (Pitch { pitchClass: B, accidental: DoubleSharp }) 1
  : Nil
  )

-- | the pitch number is the position of the pitch in the chromatic scale
-- | starting at C Natural = 0 (i.e. C is 0, C Sharp is 1 B is 11 etc.)
pitchNumber :: Pitch -> Int
pitchNumber (Pitch p) =
  let
    target =
      case p.accidental of
        Implicit ->
          Pitch { pitchClass : p.pitchClass, accidental : Natural }
        _ ->
          Pitch p
  in
    fromMaybe 0 $ lookup target chromaticScaleMap

-- | The set of keys (pitch classes and accidental) that comprise a diatonic scale
-- | in the given key signature.
diatonicScale :: KeySignature -> DiatonicScale
diatonicScale ks =
  let
    target =
      Pitch {pitchClass: ks.pitchClass, accidental: ks.accidental}
  in
    case ks.mode of
      Major ->
        majorScale target

      Ionian ->
        majorScale target

      m ->
        modalScale target ks.mode


-- | Is the pitch in the diatonic scale?
inScale :: Pitch -> DiatonicScale -> Boolean
inScale p s =
  elem p s

-- | The set of keys (pitch classes with accidental) that comprise the key signature.
keySet :: KeySignature -> KeySet
keySet ks =
 filter accidentalKey $ diatonicScale ks

-- | The set of keys (pitch classes with accidental) that comprise a modified key signature
modifiedKeySet :: ModifiedKeySignature -> KeySet
modifiedKeySet ksm =
  let
    kSet = keySet ksm.keySignature
  in
    if (null ksm.modifications) then
      kSet
  else
    foldr modifyKeySet kSet ksm.modifications

-- | Get the set of key accidentals from the (possibly modified) key (if there is one in the tune).
getKeySet :: AbcTune -> KeySet
getKeySet t =
  let
    mksig =
      getKeySig t
  in
    case mksig of
      Just ksig ->
        modifiedKeySet ksig
      Nothing ->
        Nil

-- | Is the pitch is in the KeySet?
inKeySet :: Pitch -> KeySet -> Boolean
inKeySet p ks =
  elem p ks

-- | A map (Header code => Header) for the first instance of each Header
getHeaderMap :: AbcTune -> HeaderMap
getHeaderMap t =
  let
    f :: Header -> Tuple Char Header
    f h =
      case h of
        Area _ ->
          Tuple 'A' h

        Book _ ->
          Tuple 'B' h

        Composer _ ->
          Tuple 'C' h

        Discography _ ->
          Tuple 'D' h

        FileUrl _ ->
          Tuple 'F' h

        Group _ ->
          Tuple 'G' h

        History _ ->
          Tuple 'H' h

        Instruction _ ->
          Tuple 'I' h

        Key _ ->
          Tuple 'K' h

        UnitNoteLength _ ->
          Tuple 'L' h

        Meter _ ->
          Tuple 'M' h

        Macro _ ->
          Tuple 'm' h

        Notes _ ->
          Tuple 'N' h

        Origin _ ->
          Tuple 'O' h

        Parts _ ->
          Tuple 'P' h

        Tempo _ ->
          Tuple 'Q' h

        Rhythm _ ->
          Tuple 'R' h

        Remark _ ->
          Tuple 'r' h

        Source _ ->
          Tuple  'S' h

        SymbolLine _ ->
          Tuple 's' h

        Title _ ->
          Tuple 'T' h

        UserDefined _ ->
          Tuple 'U' h

        Voice _ ->
          Tuple 'V' h

        WordsAfter _ ->
          Tuple 'W' h

        WordsAligned _ ->
          Tuple  'w' h

        ReferenceNumber _ ->
          Tuple 'X' h

        Transcription _ ->
          Tuple 'Z' h

        FieldContinuation _ ->
          Tuple '+' h

        Comment _ ->
            Tuple '-' h

        UnsupportedHeader ->
          Tuple 'u' h

    annotatedHeaders =
      map f $ reverse t.headers
  in
    fromFoldable annotatedHeaders


-- | Get the key signature (if any) from the tune.
getKeySig :: AbcTune -> Maybe ModifiedKeySignature
getKeySig tune =
  case (getHeader 'K' tune) of
    Just (Key key) ->
      Just key
    _ ->
      Nothing

-- | Get the meter
getMeter :: AbcTune -> Maybe MeterSignature
getMeter tune =
  case (getHeader 'M' tune) of
    Just (Meter maybeMeter) ->
      Just (fromMaybe (Tuple 4 4) $ maybeMeter)
    _ ->
      Nothing

getTempoSig :: AbcTune -> Maybe TempoSignature
getTempoSig tune =
  case (getHeader 'Q' tune) of
    Just (Tempo tempo) ->
      Just tempo
    _ ->
      Nothing

-- | Get the first Title (if any) from the tune.
getTitle :: AbcTune -> Maybe String
getTitle tune =
  case (getHeader 'T' tune) of
    Just (Title title) ->
      Just title
    _ ->
      Nothing

-- | Get the unit note length
getUnitNoteLength :: AbcTune -> Maybe NoteDuration
getUnitNoteLength tune =
  case (getHeader 'L' tune) of
    Just (UnitNoteLength duration) ->
      Just duration
    _ ->
      Nothing

-- | Get the header from the header code
getHeader :: Char -> AbcTune -> Maybe Header
getHeader code t =
  lookup code (getHeaderMap t)

-- | Is the key signature a sharp key or else a simple C Major key?
isCOrSharpKey :: KeySignature -> Boolean
isCOrSharpKey ksig =
  let
    kset =
      keySet ksig
    isFlat :: Pitch -> Boolean
    isFlat (Pitch p) =
      p.accidental == Flat
  in
    -- the list is empty anyway or contains only flat keys
    null $ filter isFlat kset

-- | modify a key set by adding new accidental
-- | This is ussed in order to start with a simple key signature
-- | and end up with an extended set of keys by taking into account
-- | the modifications which add specific sharos or flats
modifyKeySet :: Pitch -> KeySet -> KeySet
modifyKeySet newP ks =
  case newP of
    -- ignore naturals in incomimg key for key signatures
    Pitch { pitchClass: _, accidental: Natural}  ->
      ks
    Pitch { pitchClass: pitchClass, accidental: _ }  ->
      newP: (filter (noMatchPC pitchClass) ks)
  where
    noMatchPC :: PitchClass -> Pitch -> Boolean
    noMatchPC pc (Pitch p) =
       pc /= p.pitchClass


{-| The amount by which you increase or decrease the duration of a (possibly multiply) dotted note.
    For example A > B increases the duration of A and proportionally reduces that of B.
    A << B decreases the duration of A and increases that of B by an even greater amount.  This function
    calculates the increase or decrease.  The new duration will be given by:

    duration * (1 +/- dotfactor i)

*  i is the number of 'dot' indicators (< or >)

-}
dotFactor :: Int -> Rational
dotFactor i =
  case i of
    1 ->
      1 % 2

    2 ->
      3 % 4

    3 ->
      7 % 8

    _ ->
      0 % 1

-- temporary
lookupScale :: ChromaticScale -> Int -> Maybe Pitch
lookupScale scale idx =
  index scale idx

-- | Transpose a key signature by a given distance.
transposeKeySignatureBy :: Int -> ModifiedKeySignature -> ModifiedKeySignature
transposeKeySignatureBy i mks =
  let
    -- turn the key sig to a string pattern and look up its index
    pattern =
      Pitch { pitchClass : mks.keySignature.pitchClass, accidental : mks.keySignature.accidental }

    idx =
      pitchNumber pattern

    newIndex =
      (notesInChromaticScale + idx + i) `mod` notesInChromaticScale

    -- keep hold of the sharp / flat nature of the original scale
    scale =
      if (isCOrSharpKey mks.keySignature) then
        sharpScale
      else
        flatScale

    -- now look up the transposed key at the new index
    kar =
      fromMaybe (Pitch { pitchClass: C, accidental: Natural }) $ index scale newIndex
    -- modify the key accidentals likewise
    accs =
      map (transposeKeyAccidentalBy i) mks.modifications

    -- build the most likely enharmonic equivalent - don't use bizarre keys
    newks =
      case kar of
        Pitch p ->
          equivalentEnharmonicKeySig  p.pitchClass p.accidental mks.keySignature.mode
  in
    { keySignature: newks, modifications: accs }


-- implementation

-- | lookup for providing offsets from C in a chromatic scale
chromaticScaleMap :: Map Pitch Int
chromaticScaleMap =
  fromFoldable pitchNumbers

{- works from C major up to B major but not beyond
    (F# major requires F->E#, C# major also requires C->B#)
   "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
-}
sharpScale :: ChromaticScale
sharpScale =
      Pitch { pitchClass: C, accidental: Natural }
    : Pitch { pitchClass: C, accidental: Sharp }
    : Pitch { pitchClass: D, accidental: Natural }
    : Pitch { pitchClass: D, accidental: Sharp }
    : Pitch { pitchClass: E, accidental: Natural }
    : Pitch { pitchClass: F, accidental: Natural }
    : Pitch { pitchClass: F, accidental: Sharp }
    : Pitch { pitchClass: G, accidental: Natural }
    : Pitch { pitchClass: G, accidental: Sharp }
    : Pitch { pitchClass: A, accidental: Natural }
    : Pitch { pitchClass: A, accidental: Sharp }
    : Pitch { pitchClass: B, accidental: Natural }
    : Nil

-- "B#", "C#", "D", "D#", "E", "E#", "F#", "G", "G#", "A", "A#", "B"
extremeSharpScale :: ChromaticScale
extremeSharpScale =
  let
    f pc =
      case pc of

        Pitch { pitchClass: F, accidental: Natural } ->
          Pitch { pitchClass: E, accidental: Sharp }

        Pitch { pitchClass: C, accidental: Natural } ->
          Pitch { pitchClass: B, accidental: Sharp }

        _ ->
          pc
  in
    map f sharpScale

{- works from C major down to Db major but not beyond
   (Gb major requires B->Cb, Cb major also requires E->Fb)
import Data.Newtype (unwrap)
   "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"
-}
flatScale :: ChromaticScale
flatScale =
    ( Pitch { pitchClass: C, accidental: Natural }
    : Pitch { pitchClass: D, accidental: Flat }
    : Pitch { pitchClass: D, accidental: Natural }
    : Pitch { pitchClass: E, accidental: Flat }
    : Pitch { pitchClass: E, accidental: Natural }
    : Pitch { pitchClass: F, accidental: Natural }
    : Pitch { pitchClass: G, accidental: Flat }
    : Pitch { pitchClass: G, accidental: Natural }
    : Pitch { pitchClass: A, accidental: Flat }
    : Pitch { pitchClass: A, accidental: Natural }
    : Pitch { pitchClass: B, accidental: Flat }
    : Pitch { pitchClass: B, accidental: Natural }
    : Nil
    )

-- | "C", "Db", "D", "Eb", "Fb", "F", "Gb", "G", "Ab", "A", "Bb", "Cb"
extremeFlatScale :: ChromaticScale
extremeFlatScale =
  let
    f pc =
      case pc of
        Pitch { pitchClass: E, accidental: Natural } ->
          Pitch { pitchClass: F, accidental: Flat }

        Pitch { pitchClass: B, accidental: Natural } ->
          Pitch { pitchClass: C, accidental: Flat }

        _ ->
          pc
  in
    map f flatScale



{- enharmonic equivalence of key classes - don't use bizarre sharp keys when we have reasonable flat ones -}
equivalentEnharmonic :: Pitch -> Pitch
equivalentEnharmonic k =
  case k of
    Pitch { pitchClass: A, accidental: Sharp } ->
      Pitch { pitchClass: B, accidental: Flat }

    Pitch { pitchClass: C, accidental: Sharp } ->
      Pitch { pitchClass: D, accidental: Flat }

    Pitch { pitchClass: D, accidental: Sharp } ->
      Pitch { pitchClass: E, accidental: Flat }

    Pitch { pitchClass: G, accidental: Sharp } ->
      Pitch { pitchClass: A, accidental: Flat }

    _ ->
      k


sharpScaleEquivalent :: Pitch -> Pitch
sharpScaleEquivalent (Pitch p) =
  case p.accidental of
    Flat ->
      let
        index =
          fromMaybe 0 $ elemIndex (Pitch p) flatScale
      in
        lookUpScale sharpScale index

    _ ->
      (Pitch p)

{- enharmonic equivalence of full key signatures - don't use bizarre minor flat keys when we have reasonable sharp ones
   and vice versa.  Check both Major and Monor key signatures.
-}
equivalentEnharmonicKeySig :: PitchClass -> Accidental -> Mode -> KeySignature
equivalentEnharmonicKeySig pc a m =
  let
    pattern = Tuple (Pitch { pitchClass: pc, accidental: a }) m
  in
    case pattern of
      -- major key signatures
      Tuple (Pitch { pitchClass: A, accidental: Sharp }) Major ->
        { pitchClass: B, accidental: Flat, mode: Major }

      Tuple (Pitch { pitchClass: D, accidental: Sharp}) Major  ->
        { pitchClass: E, accidental: Flat, mode: Major }

      Tuple (Pitch { pitchClass: G, accidental: Sharp }) Major   ->
        { pitchClass: A, accidental: Flat, mode: Major }

      -- minor key signatures
      Tuple (Pitch { pitchClass: G, accidental: Flat}) Minor   ->
        { pitchClass: F, accidental: Sharp, mode: Minor }

      Tuple (Pitch { pitchClass: D, accidental: Flat})  Minor  ->
        { pitchClass: C, accidental: Sharp, mode: Minor }

      Tuple (Pitch { pitchClass: A, accidental: Flat}) Minor ->
        { pitchClass: G, accidental: Sharp, mode: Minor }

      _ ->
       { pitchClass: pc, accidental: a, mode: m }


{- The major scale intervals are 2,2,1,2,2,2,1
   This reperesents the accumulated intervals starting from 0
   which are used as offsets into a representation of a major scale
-}
majorIntervalOffsets :: Intervals
majorIntervalOffsets =
  (0: 2: 4: 5: 7: 9: 11: Nil)


-- rotate the chromatic scale, starting from the supplied target character

--- JMW!!!
rotateFrom :: Pitch -> ChromaticScale -> ChromaticScale
rotateFrom target scale =
  let
    idx =
      fromMaybe 0 $ elemIndex target scale

    left = slice 0 idx scale
    right = slice idx (length scale) scale
  in
    right <> left

-- rotate an interval pattern to the left by the given amount
rotateLeftBy :: Int -> Intervals -> Intervals
rotateLeftBy idx ls =
  let
    left = slice 0 idx ls
    right = slice idx (length ls) ls
  in
    right <> left

{- find the pitch class at a given position in the scale
   modulate the index to be in the range 0 <= index < notesInChromaticScale
   where a negative value rotates left from the maximum value in the scale
-}
lookUpScale :: ChromaticScale -> Int -> Pitch
lookUpScale s i =
  let
    modi =
      i `mod` notesInChromaticScale

    idx =
      if (modi < 0) then
        notesInChromaticScale - modi
      else
        modi
  in
    fromMaybe (Pitch { pitchClass: C, accidental: Natural }) $ index s idx

-- provide the Major scale for the pitch class
majorScale :: Pitch -> DiatonicScale
majorScale target =
  let
    chromaticScale =
      if (target ==Pitch { pitchClass: G, accidental: Flat }
          || target == Pitch { pitchClass: C, accidental: Flat }) then
            extremeFlatScale
      else if (isFlatMajorKey target) then
        flatScale
      else if (target == Pitch { pitchClass: F, accidental: Sharp }
               || target == Pitch { pitchClass: C, accidental: Sharp }) then
        extremeSharpScale
      else
        sharpScale
    f =
      lookUpScale (rotateFrom target chromaticScale)
  in
    -- map f (partialSum majorIntervals)
    map f majorIntervalOffsets

-- provide a Modal scale for the pitch class
modalScale :: Pitch -> Mode -> DiatonicScale
modalScale target mode =
  let
    distance =
      case mode of
        Minor ->
          3

        Major ->
          0

        _ ->
          modalDistance mode

    index =
      fromMaybe 0 $ elemIndex target sharpScale

    majorKeyIndex =
      (index + distance) `mod` notesInChromaticScale

    majorKey =
      lookUpScale sharpScale majorKeyIndex
  in
    majorScale (equivalentEnharmonic majorKey)


-- | normalise a modal key signature to that of the equivalent major key
-- | Maybe, once this is completed and tested, implement modalScale in terms of this
normaliseModalKey :: KeySignature -> KeySignature
normaliseModalKey ks =
  let
    distance =
      modalDistance ks.mode

    sourceAccidental =
      ks.accidental

    scale =
      case ks.accidental of
        Sharp ->
          sharpScale

        Flat ->
          flatScale

        _ ->
          case ks.pitchClass of
            F ->
              flatScale

            _ ->
              sharpScale
    keyAccidental =
      Pitch { pitchClass:  ks.pitchClass, accidental: sourceAccidental }

    idx =
      fromMaybe 0 $ elemIndex keyAccidental scale

    majorKeyIndex =
      (idx + distance) `mod` notesInChromaticScale

    majorKeyAcc =
      lookUpScale scale majorKeyIndex
  in
    if (0 == distance) then
      ks
    else
      case majorKeyAcc of
        Pitch mka ->
          { pitchClass: mka.pitchClass
          , accidental: mka.accidental
          , mode: Major
          }

modalDistance :: Mode -> Int
modalDistance mode =
  case mode of
    -- the distance to move right round the major scale
    Dorian ->
      10

    Phrygian ->
      8

    Lydian ->
      7

    Mixolydian ->
      5

    Aeolian ->
      3

    Minor ->
      3

    Locrian ->
      1

    _ ->
      0


{- return true if the key contains a sharp or flat accidental -}
accidentalKey :: Pitch -> Boolean
accidentalKey (Pitch p) =
  p.accidental /= Natural

{- return true if the key represents a flat major key -}
isFlatMajorKey :: Pitch -> Boolean
isFlatMajorKey (Pitch target) =
  case target.accidental of
    Natural ->
      (target.pitchClass == F)
    a ->
      (a == Flat)


{- transpose a key accidental  (a key signature modifier)
   not finished
-}
transposeKeyAccidentalBy :: Int -> Pitch -> Pitch
transposeKeyAccidentalBy i (Pitch p) =
  let
    pattern =
      show p.pitchClass

    idx =
      fromMaybe 0 $ lookup (Pitch p) chromaticScaleMap

    scaleModifier :: { modifier :: Int, scale :: ChromaticScale }
    scaleModifier =
      case p.accidental of
        Sharp ->
         { modifier: 1, scale: sharpScale }

        Flat ->
         { modifier: -1, scale: flatScale }

        DoubleSharp ->
         { modifier: 2, scale: sharpScale }

        DoubleFlat ->
         { modifier: -2, scale: flatScale }

        _ ->   -- Natural, Implicit
         { modifier: 0, scale: sharpScale }
  in
    fromMaybe (Pitch { pitchClass: C, accidental: Natural }) $
       index scaleModifier.scale (idx + scaleModifier.modifier + i)
