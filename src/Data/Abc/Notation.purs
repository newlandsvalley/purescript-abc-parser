module Data.Abc.Notation
        ( MidiPitch
        , MidiTick
        , AbcTempo
        , NoteTime
        , DiatonicScale
        , HeaderMap
        , notesInChromaticScale
        , standardMidiTick
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
        , diatonicScale
        , inScale
        , isCOrSharpKey
        , accidentalImplicitInKey
        , dotFactor
        , noteTicks
        , chordalNoteTicks
        , toMidiPitch
        , midiTempo
        , noteDuration
        , chordalNoteDuration
        , transposeKeySignatureBy
        , normaliseModalKey
        ) where


import Data.Abc
import Data.Abc.Accidentals as Accidentals
import Data.Foldable (oneOf)
import Data.Int (round)
import Data.List (List(..), (:), elem, elemIndex, foldr, filter, index, length, null, reverse, slice)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Rational (Rational, fromInt, toNumber, (%))
import Data.Tuple (Tuple(..))
import Prelude (($), (<>), (+), (-), (*), (/), (<), (==), (/=), (||), show, negate, map, mod)

-- | A diatonic scale presented as a list of notes in the scale.
type DiatonicScale =
    List KeyAccidental

-- | A chromatic (12-note) scale -}
type ChromaticScale =
    List KeyAccidental

type Intervals =
    List Int

-- | The pitch of a note expressed as a MIDI interval.
type MidiPitch =
    Int

-- | A MIDI tick - used to give a note duration.
type MidiTick =
    Int

-- | The time taken when a note is played before the next note.
type NoteTime =
    Number

{-| The tempo when the tune is being played. This is usually represented
as (for example) 1/4 = 120 - i.e. 120 querter notes per minute.

* tempoNoteLength - the note length of a tempo definition
* bpm - the beats per minute of a tempo Definition
* unitNoteLength - the length of a 'unit note' in the ABC definition
-}
type AbcTempo =
    { tempoNoteLength :: Rational
    , bpm :: Int
    , unitNoteLength :: Rational
    }


-- | A representation of the ABC headers as a Map, taking the first definition
-- | of any header if multiple definitions are present in the ABC.
type HeaderMap =
    Map Char Header

-- EXPORTED FUNCTIONS


-- | A standard MIDI tick - we use 1/4 note = 480 ticks.
standardMidiTick :: MidiTick
standardMidiTick =
  480


-- | Number of notes in a chromatic scale (i.e. 12)
notesInChromaticScale :: Int
notesInChromaticScale =
  12


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


-- | Is the KeyAccidental is in the KeySet?
inKeySet :: KeyAccidental -> KeySet -> Boolean
inKeySet ka ks =
  elem ka ks


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


-- | The set of keys (pitch classes and accidental) that comprise a diatonic scale
-- | in the given key signature.
diatonicScale :: KeySignature -> DiatonicScale
diatonicScale ks =
  let
    accidental =
      case ks.accidental of
        Just a ->
          a

        Nothing ->
          Natural

    target =
      KeyAccidental {pitchClass: ks.pitchClass, accidental: accidental}
  in
    case ks.mode of
      Major ->
        majorScale target

      Ionian ->
        majorScale target

      m ->
        modalScale target ks.mode


-- | Is the KeyAccidental in the diatonic scale?
inScale :: KeyAccidental -> DiatonicScale -> Boolean
inScale ka s =
  elem ka s

-- | Is the key signature a sharp key or else a simple C Major key?
isCOrSharpKey :: KeySignature -> Boolean
isCOrSharpKey ksig =
  let
    kset =
      keySet ksig
    isFlat :: KeyAccidental -> Boolean
    isFlat keyAccidental =
      (unwrap keyAccidental).accidental == Flat
  in
    -- the list is empty anyway or contains only flat keys
    null $ filter isFlat kset

-- | Return an accidental if it is implicitly there in the (modified) key signature
-- | attached to the pitch class of the note. In ABC, notes generally inherit
-- | their (sharp, flat or natural) accidental nature from the key signature.
accidentalImplicitInKey :: PitchClass -> ModifiedKeySignature -> Maybe Accidental
accidentalImplicitInKey pc mks =
  let
    keyset =
      modifiedKeySet mks

    accidentals =
      Accidentals.fromKeySet keyset
  in
    lookup pc accidentals


-- | modify a key set by adding new accidental
-- | This is ussed in order to start with a simple key signature
-- | and end up with an extended set of keys by taking into account
-- | the modifications which add specific sharos or flats
modifyKeySet :: KeyAccidental -> KeySet -> KeySet
modifyKeySet newKeyA ks =
  case newKeyA of
    -- ignore naturals in incomimg key for key signatures
    KeyAccidental { pitchClass: _, accidental: Natural}  ->
      ks
    KeyAccidental { pitchClass: pitchClass, accidental: _ }  ->
        newKeyA : (filter (noMatchPC pitchClass) ks)
  where
    noMatchPC :: PitchClass -> KeyAccidental -> Boolean
    noMatchPC pc nka =
       pc /= (unwrap nka).pitchClass


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


-- | Find a real world note duration by translating an ABC note duration using
-- | the tune's tempo and unit note length.
noteDuration :: AbcTempo -> Rational -> NoteTime
noteDuration t n =
  (60.0 * (toNumber t.unitNoteLength) * (toNumber n))
        / ((toNumber t.tempoNoteLength) * (toNumber $ fromInt t.bpm))


-- | Find a real world duration of a note in a chord by translating an ABC note duration together with
-- | the chord duration using the tune's tempo and unit note length.
chordalNoteDuration :: AbcTempo -> Rational -> Rational -> NoteTime
chordalNoteDuration t note chord =
  noteDuration t note * (toNumber chord)

-- MIDI support

-- | Calculate a MIDI note duration from the note length.
-- | Assume a standard unit note length of 1/4 and a standard number of ticks per unit (1/4) note of 480.
noteTicks :: Rational -> MidiTick
noteTicks n =
  -- (standardMidiTick * (numerator n)) // (denominator n)
  round $ toNumber $ n * (fromInt standardMidiTick)


-- | Find the MIDI duration of a note within a chord in standard ticks
-- |    (1/4 note == 480 ticks)
chordalNoteTicks :: Rational -> Rational -> MidiTick
chordalNoteTicks note chord =
  round $ toNumber $ note * chord * (fromInt standardMidiTick)


{-| Convert an ABC note pitch to a MIDI pitch.

*  AbcNote - the note in question
*  ModifiedKeySignature - the key signature (possibly modified by extra accidentals)
*  Accidentals - any notes in this bar which have previously been set explicitly to an accidental which are thus inherited by this note
*  MidiPitch - the resulting pitch of the MIDI note

-}
toMidiPitch :: AbcNote -> ModifiedKeySignature -> Accidentals.Accidentals -> MidiPitch
toMidiPitch n mks barAccidentals =
  (n.octave * notesInChromaticScale) + midiPitchOffset n mks barAccidentals



{-
   midiTempo algorithm is:

   t.bpm beats occupy 1 minute or 60 * 10^16 μsec
   1 bpm beat occupies 60 * 10^16/t.bpm μsec

   but we use a standard beat of 1 unit when writing a note, whereas the bpm measures a tempo note length of
   t.unitNoteLength/t.tempoNoteLength
   i.e.
   1 whole note beat occupies 60 * 10^16/t.bpm * t.unl/t.tnl μsec

-}


-- | The MIDI tempo measured in microseconds per beat.
-- | JMW!!! check
midiTempo :: AbcTempo -> Int
midiTempo t =
  let
    relativeNoteLength =
      t.unitNoteLength / t.tempoNoteLength
  in
    round ((60.0 * 1000000.0 * (toNumber relativeNoteLength)) / (toNumber $ fromInt t.bpm))

    -- Basics.round ((60.0 * 1000000 * Basics.toFloat (numerator relativeNoteLength)) / (Basics.toFloat t.bpm * Basics.toFloat (denominator relativeNoteLength)))


-- temporary
lookupScale :: ChromaticScale -> Int -> Maybe KeyAccidental
lookupScale scale idx =
  index scale idx

{-
  let
    result = index scale idx
  in
    trace "lookup chromatic scale at" \_ ->
    traceShow idx \_ ->
    traceShow result \_ ->
    result
-}

-- | Transpose a key signature by a given distance.
transposeKeySignatureBy :: Int -> ModifiedKeySignature -> ModifiedKeySignature
transposeKeySignatureBy i mks =
  let
    -- turn the key sig to a string pattern and look up its index
    pattern =
      (show mks.keySignature.pitchClass) <> (accidentalPattern mks.keySignature.accidental)

    idx =
      fromMaybe 0 $ lookup pattern chromaticScaleMap

    newIndex =
      (notesInChromaticScale + idx + i) `mod` notesInChromaticScale

    -- keep hold of the sharp / flat nature of the original scale
    scale =
      if (isCOrSharpKey mks.keySignature) then
        sharpScale
      else
        flatScale

    -- now look up the transposed key at the new index
    ka =
      fromMaybe (KeyAccidental { pitchClass: C, accidental: Natural }) $ index scale newIndex
      -- fromMaybe (KeyAccidental { pitchClass: C, accidental: Natural }) $ lookupScale scale newIndex

    -- modify the key accidentals likewise
    accs =
      map (transposeKeyAccidentalBy i) mks.modifications

    -- build the most likely enharmonic equivalent - don't use bizarre keys
    newks =
      equivalentEnharmonicKeySig  (unwrap ka).pitchClass (unwrap ka).accidental mks.keySignature.mode
  in
    { keySignature: newks, modifications: accs }



-- implementation
{- works from C major up to B major but not beyond
    (F# major requires F->E#, C# major also requires C->B#)
   "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
-}
sharpScale :: ChromaticScale
sharpScale =
      KeyAccidental { pitchClass: C, accidental: Natural }
    : KeyAccidental { pitchClass: C, accidental: Sharp }
    : KeyAccidental { pitchClass: D, accidental: Natural }
    : KeyAccidental { pitchClass: D, accidental: Sharp }
    : KeyAccidental { pitchClass: E, accidental: Natural }
    : KeyAccidental { pitchClass: F, accidental: Natural }
    : KeyAccidental { pitchClass: F, accidental: Sharp }
    : KeyAccidental { pitchClass: G, accidental: Natural }
    : KeyAccidental { pitchClass: G, accidental: Sharp }
    : KeyAccidental { pitchClass: A, accidental: Natural }
    : KeyAccidental { pitchClass: A, accidental: Sharp }
    : KeyAccidental { pitchClass: B, accidental: Natural }
    : Nil

-- "B#", "C#", "D", "D#", "E", "E#", "F#", "G", "G#", "A", "A#", "B"

extremeSharpScale :: ChromaticScale
extremeSharpScale =
  let
    f pc =
      case pc of

        KeyAccidental { pitchClass: F, accidental: Natural } ->
          KeyAccidental { pitchClass: E, accidental: Sharp }

        KeyAccidental { pitchClass: C, accidental: Natural } ->
          KeyAccidental { pitchClass: B, accidental: Sharp }

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
    ( KeyAccidental { pitchClass: C, accidental: Natural }
    : KeyAccidental { pitchClass: D, accidental: Flat }
    : KeyAccidental { pitchClass: D, accidental: Natural }
    : KeyAccidental { pitchClass: E, accidental: Flat }
    : KeyAccidental { pitchClass: E, accidental: Natural }
    : KeyAccidental { pitchClass: F, accidental: Natural }
    : KeyAccidental { pitchClass: G, accidental: Flat }
    : KeyAccidental { pitchClass: G, accidental: Natural }
    : KeyAccidental { pitchClass: A, accidental: Flat }
    : KeyAccidental { pitchClass: A, accidental: Natural }
    : KeyAccidental { pitchClass: B, accidental: Flat }
    : KeyAccidental { pitchClass: B, accidental: Natural }
    : Nil
    )



-- "C", "Db", "D", "Eb", "Fb", "F", "Gb", "G", "Ab", "A", "Bb", "Cb"


extremeFlatScale :: ChromaticScale
extremeFlatScale =
  let
    f pc =
      case pc of
        KeyAccidental { pitchClass: E, accidental: Natural } ->
          KeyAccidental { pitchClass: F, accidental: Flat }

        KeyAccidental { pitchClass: B, accidental: Natural } ->
          KeyAccidental { pitchClass: C, accidental: Flat }

        _ ->
          pc
  in
    map f flatScale



{- enharmonic equivalence of key classes - don't use bizarre sharp keys when we have reasonable flat ones -}
equivalentEnharmonic :: KeyAccidental -> KeyAccidental
equivalentEnharmonic k =
  case k of
    KeyAccidental { pitchClass: A, accidental: Sharp } ->
      KeyAccidental { pitchClass: B, accidental: Flat }

    KeyAccidental { pitchClass: C, accidental: Sharp } ->
      KeyAccidental { pitchClass: D, accidental: Flat }

    KeyAccidental { pitchClass: D, accidental: Sharp } ->
      KeyAccidental { pitchClass: E, accidental: Flat }

    KeyAccidental { pitchClass: G, accidental: Sharp } ->
      KeyAccidental { pitchClass: A, accidental: Flat }

    _ ->
      k


sharpScaleEquivalent :: KeyAccidental -> KeyAccidental
sharpScaleEquivalent ka =
  case (unwrap ka).accidental of
    Flat ->
      let
        -- JMW!!!
        index =
          fromMaybe 0 $ elemIndex ka flatScale
      in
        lookUpScale sharpScale index

    _ ->
      ka

{- enharmonic equivalence of full key signatures - don't use bizarre minor flat keys when we have reasonable sharp ones
   and vice versa.  Check both Major and Monor key signatures.
-}
equivalentEnharmonicKeySig :: PitchClass -> Accidental -> Mode -> KeySignature
equivalentEnharmonicKeySig pc a m =
  let
    pattern = Tuple (KeyAccidental { pitchClass: pc, accidental: a }) m
  in
    case pattern of
      -- major key signatures
      Tuple (KeyAccidental { pitchClass: A, accidental: Sharp }) Major ->
        { pitchClass: B, accidental: Just Flat, mode: Major }

      Tuple (KeyAccidental { pitchClass: D, accidental: Sharp}) Major  ->
        { pitchClass: E, accidental: Just Flat, mode: Major }

      Tuple (KeyAccidental { pitchClass: G, accidental: Sharp }) Major   ->
        { pitchClass: A, accidental: Just Flat, mode: Major }

      -- minor key signatures
      Tuple (KeyAccidental { pitchClass: G, accidental: Flat}) Minor   ->
        { pitchClass: F, accidental: Just Sharp, mode: Minor }

      Tuple (KeyAccidental { pitchClass: D, accidental: Flat})  Minor  ->
        { pitchClass: C, accidental: Just Sharp, mode: Minor }

      Tuple (KeyAccidental { pitchClass: A, accidental: Flat}) Minor ->
        { pitchClass: G, accidental: Just Sharp, mode: Minor }

      _ ->
       { pitchClass: pc, accidental: Just a, mode: m }


{- The major scale intervals are 2,2,1,2,2,2,1
   This reperesents the accumulated intervals starting from 0
   which are used as offsets into a representation of a major scale
-}
majorIntervalOffsets :: Intervals
majorIntervalOffsets =
  (0: 2: 4: 5: 7: 9: 11: Nil)

-- | lookup for providing offsets from C in a chromatic scale
chromaticScaleMap :: Map String Int
chromaticScaleMap =
    fromFoldable
        [ Tuple "C" 0
        , Tuple  "C#" 1
        , Tuple  "Db" 1
        , Tuple  "D" 2
        , Tuple  "D#" 3
        , Tuple  "Eb" 3
        , Tuple  "E" 4
        , Tuple  "F" 5
        , Tuple  "F#" 6
        , Tuple  "Gb" 6
        , Tuple  "G" 7
        , Tuple  "G#" 8
        , Tuple  "Ab" 8
        , Tuple  "A" 9
        , Tuple  "A#" 10
        , Tuple  "Bb" 10
        , Tuple  "B" 11
        ]

-- rotate the chromatic scale, starting from the supplied target character

--- JMW!!!
rotateFrom :: KeyAccidental -> ChromaticScale -> ChromaticScale
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
lookUpScale :: ChromaticScale -> Int -> KeyAccidental
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
    fromMaybe (KeyAccidental { pitchClass: C, accidental: Natural }) $ index s idx

-- provide the Major scale for the pitch class
majorScale :: KeyAccidental -> DiatonicScale
majorScale target =
  let
    chromaticScale =
      if (target == KeyAccidental { pitchClass: G, accidental: Flat }
          || target == KeyAccidental { pitchClass: C, accidental: Flat }) then
            extremeFlatScale
      else if (isFlatMajorKey target) then
        flatScale
      else if (target == KeyAccidental { pitchClass: F, accidental: Sharp }
               || target == KeyAccidental { pitchClass: C, accidental: Sharp }) then
        extremeSharpScale
      else
        sharpScale
    f =
      lookUpScale (rotateFrom target chromaticScale)
  in
    -- map f (partialSum majorIntervals)
    map f majorIntervalOffsets

-- provide a Modal scale for the pitch class
-- JMW!!!
modalScale :: KeyAccidental -> Mode -> DiatonicScale
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


{-| normalise a modal key signature to that of the equivalent major key

  Maybe, once this is completed and tested, implement modalScale in terms of this

-}
normaliseModalKey :: KeySignature -> KeySignature
normaliseModalKey ks =
  let
    distance =
      modalDistance ks.mode

    sourceAccidental =
      maccToAcc ks.accidental

    scale =
      case ks.accidental of
        Just Sharp ->
          sharpScale

        Just Flat ->
          flatScale

        _ ->
          case ks.pitchClass of
            F ->
              flatScale

            _ ->
              sharpScale
    keyAccidental =
      KeyAccidental { pitchClass:  ks.pitchClass, accidental: sourceAccidental }

    idx =
      fromMaybe 0 $ elemIndex keyAccidental scale

    majorKeyIndex =
      (idx + distance) `mod` notesInChromaticScale

    majorKeyAcc =
      lookUpScale scale majorKeyIndex

    targetAccidental =
      accToMacc (unwrap majorKeyAcc).accidental
  in
    if (0 == distance) then
      ks
    else
      { pitchClass: (unwrap majorKeyAcc).pitchClass
      , accidental: targetAccidental
      , mode: Major
      }

{- convert a Maybe Accidental (used in key signatures)
   to an explict accidental (used in scales) where the
   explict form uses Natural
-}
maccToAcc :: Maybe Accidental -> Accidental
maccToAcc macc =
   fromMaybe Natural macc

{- convert an explict accidental (used in scales)
   to a Maybe Accidental (used in key signatures) where the
   explict form uses Natural
-}
accToMacc :: Accidental -> Maybe Accidental
accToMacc acc =
  case acc of
    Sharp ->
      Just Sharp

    Flat ->
      Just Flat

    _ ->
      Nothing

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
accidentalKey :: KeyAccidental -> Boolean
accidentalKey ka =
  (unwrap ka).accidental /= Natural

{- return true if the key represents a flat major key -}
isFlatMajorKey :: KeyAccidental -> Boolean
isFlatMajorKey target =
  case (unwrap target).accidental of
    Natural ->
      ((unwrap target).pitchClass == F)
    a ->
      (a == Flat)

{- convert an AbcNote (pich class and accidental) to a pitch offset in a chromatic scale -}
midiPitchOffset :: AbcNote -> ModifiedKeySignature -> Accidentals.Accidentals -> Int
midiPitchOffset n mks barAccidentals =
  let
    inBarAccidental =
      Accidentals.lookup n.pitchClass barAccidentals

    inKeyAccidental =
      accidentalImplicitInKey n.pitchClass mks

    -- look first for an explicit note accidental, then for an explicit for the same note that occurred earlier in the bar and
    -- finally look for an implicit accidental attached to this key signature
    maybeAccidental =
      oneOf ( n.accidental:  inBarAccidental: inKeyAccidental: Nil )

    accidental =
      accidentalPattern maybeAccidental

    pattern =
      (show n.pitchClass) <> accidental
  in
    fromMaybe 0 (lookup pattern chromaticScaleMap)

{- turn an optional accidental into a string pattern for use in lookups -}
accidentalPattern :: Maybe Accidental -> String
accidentalPattern ma =
  let
    f a =
      case a of
        Sharp ->
          "#"
        Flat ->
          "b"
        _ ->
          ""
  in
    fromMaybe "" $ map f ma

{- transpose a key accidental  (a key signature modifier)
   not finished
-}
transposeKeyAccidentalBy :: Int -> KeyAccidental -> KeyAccidental
transposeKeyAccidentalBy i ka =
  let
    pattern =
      show (unwrap ka).pitchClass

    idx =
      fromMaybe 0 $ lookup pattern chromaticScaleMap

    scaleModifier :: { modifier :: Int, scale :: ChromaticScale }
    scaleModifier =
      case (unwrap ka).accidental of
        Sharp ->
         { modifier: 1, scale: sharpScale }

        Flat ->
         { modifier: -1, scale: flatScale }

        DoubleSharp ->
         { modifier: 2, scale: sharpScale }

        DoubleFlat ->
         { modifier: -2, scale: flatScale }

        Natural ->
         { modifier: 0, scale: sharpScale }
  in
    fromMaybe (KeyAccidental { pitchClass: C, accidental: Natural }) $
       index scaleModifier.scale (idx + scaleModifier.modifier + i)
