-- | ABC Key Signatures and their associated scales and details of signature.
-- | The individual (sharp or flat) keys that comprise each key signature
-- | across all the modes in western music.
module Data.Abc.KeySignature
  ( getKeySig 
  , getKeyProps
  , keySet
  , inKeySet
  , modifiedKeySet
  , getKeySet
  , notesInChromaticScale
  , diatonicScale
  , defaultKey  
  , isCOrSharpKey
  , normaliseModalKey
  , transposeKeySignatureBy
  , pitchNumbers
  , pitchNumber
  ) where

import Data.Abc (AbcTune, Accidental(..), AmorphousProperties, KeySet, KeySignature, Mode(..), ModifiedKeySignature, Pitch(..), PitchClass(..))
import Data.Abc.Optics (_headers, _properties, _ModifiedKeySignature)
import Data.Array (index, elemIndex, head, drop, take, filter, toUnfoldable)
import Data.Enum (succ, pred)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
import Data.List (List(..), (:), null, foldr)
import Data.List (elem, filter) as L
import Data.Map (Map, empty, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Show, map, mod, show, ($), (&&), (+), (/=), (<), (<>), (==), (||), (<<<))

-- Internal data structures
-- a note on a piano keyboard. The virtue of this representation is that we
-- don't (yet)commit to a particular (sharp or flat) representation
data PianoKey
  = White PitchClass
  | Black PitchClass PitchClass

derive instance eqPianoKey :: Eq PianoKey
derive instance ordPianoKey :: Ord PianoKey

instance showPianoKey :: Show PianoKey where
  show (White p) = "white: " <> (show p)
  show (Black p q) = "black: " <> (show p) <> " " <> (show q)

-- API

-- | Get the key signature (if any) from the tune.
-- | For more flexibility, you should use the _ModifiedKeySignature optic.
getKeySig :: AbcTune -> Maybe ModifiedKeySignature
getKeySig tune =
  firstOf (_headers <<< traversed <<< _ModifiedKeySignature) tune

-- | Get the key signature properties (if any) from the tune.
getKeyProps :: AbcTune -> AmorphousProperties
getKeyProps tune =
  case (firstOf (_headers <<< traversed <<< _ModifiedKeySignature <<< _properties) tune) of
    Just props -> props
    _ -> (empty :: AmorphousProperties)

-- | The set of keys (pitches) that comprise the key signature.
keySet :: KeySignature -> KeySet
keySet ks =
  let
    -- the key signature in terms of a PianoKey
    pianoKeySignature = buildPianoKey
      ( Pitch
          { pitchClass: ks.pitchClass
          , accidental: ks.accidental
          }
      )
    Tuple tonic blackKeys = blackKeySet pianoKeySignature ks.mode
    -- decide how we translate the black keys in the given context
    isFlatCtx =
      case tonic of
        White F -> true -- F Natural uses flat keys
        White _ -> false -- all other Natural key signatures use sharp (or no) keys
        _ -> true -- we'll ignore F# for the time being - all black note naturals use flat keys
    -- generate the basic keys
    basicKeySet = toUnfoldable $ map (pianoKeyToPitch isFlatCtx) blackKeys
  in
    -- special-case F# (which has an E#)
    if (isFSharp ks) then
      fSharpKeySet
    else
      -- special case Gb (which has a Cb)
      case tonic of
        Black F G ->
          -- need to mend this line with a modify
          (Pitch { pitchClass: C, accidental: Flat }) : basicKeySet
        _ ->
          basicKeySet

-- | Is the pitch is in the KeySet?
inKeySet :: Pitch -> KeySet -> Boolean
inKeySet p ks =
  L.elem p ks

-- | The set of keys (pitch classes with accidental) that comprise a modified key signature
-- | (i.e. those signatures that don't represent classical western modes such as,
-- | for example, Klezmer or Balkan music.)
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
  case (getKeySig t) of
    Just ksig ->
      modifiedKeySet ksig
    Nothing ->
      Nil 

-- constants

-- the intervals in a diatonic (major scale) are: [2, 2, 1, 2, 2, 2, 1]
-- this represents the cumulative position of each note in such a scale
-- (of seven notes)
diatonicScaleOffsets :: Array Int
diatonicScaleOffsets = [ 0, 2, 4, 5, 7, 9, 11 ]

-- The notes of a chromatic scale (on a piano) starting at C
-- This is sufficient to recognize all (major) key signatures except
-- Gb/F# which require, respectively, Cb or E#, neither of which
-- are representable like this and will have to be treated specially
pianoOctave :: Array PianoKey
pianoOctave =
  [ White C
  , Black C D
  , White D
  , Black D E
  , White E
  , White F
  , Black F G
  , White G
  , Black G A
  , White A
  , Black A B
  , White B
  ]

-- special-case the F# Scale
-- which we can't represent in our internal structure
fSharpScale :: KeySet
fSharpScale =
  Pitch { pitchClass: F, accidental: Sharp }
    : Pitch { pitchClass: G, accidental: Sharp }
    : Pitch { pitchClass: A, accidental: Sharp }
    : Pitch { pitchClass: B, accidental: Natural }
    : Pitch { pitchClass: C, accidental: Sharp }
    : Pitch { pitchClass: D, accidental: Sharp }
    : Pitch { pitchClass: E, accidental: Sharp }
    : Nil

-- ditto for the F# key signature
fSharpKeySet :: KeySet
fSharpKeySet =
  L.filter (\(Pitch p) -> p.accidental == Sharp) fSharpScale

-- F# needs to be special-cased
isFSharp :: KeySignature -> Boolean
isFSharp ks =
  ks.pitchClass == F && ks.accidental == Sharp && (ks.mode == Major || ks.mode == Ionian)

-- Exported Functioms

-- | the number of notes in a chromatic scale (12)
notesInChromaticScale :: Int
notesInChromaticScale =
  12

-- | The default key is C Major (with no accidental modifiers or other properties)
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }
  , modifications: Nil
  , properties: empty
  }

-- | The set of keys (pitches) that comprise the diatonic scale governed by
-- | the key signature.
diatonicScale :: KeySignature -> KeySet
diatonicScale ks =
  let
    -- the key signature in terms of a PianoKey
    pianoKeySignature = buildPianoKey
      ( Pitch
          { pitchClass: ks.pitchClass
          , accidental: ks.accidental
          }
      )
    Tuple tonic allKeys = pianoKeyScale pianoKeySignature ks.mode
    -- decide how we translate the black keys in the given context
    isFlatCtx =
      case tonic of
        White F -> true -- F Natural uses flat keys
        White _ -> false -- all other Natural key signatures use sharp (or no) keys
        _ -> true -- we'll ignore F# for the time being - all black note naturals use flat keys
    -- generate the basic keys
    basicKeySet = toUnfoldable $ map (pianoKeyToPitch isFlatCtx) allKeys
  in
    -- special-case F# (which has an E#)
    if (isFSharp ks) then
      fSharpScale
    else
      -- special case Gb (which has a Cb)
      case tonic of
        Black F G ->
          renameBNatural basicKeySet
        _ ->
          basicKeySet

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
    null $ L.filter isFlat kset

-- | normalise a modal key signature to its equivalent major key signature
normaliseModalKey :: KeySignature -> KeySignature
normaliseModalKey ks =
  let
    -- convert key sig to a piano key
    pianoKeySignature = buildPianoKey
      ( Pitch
          { pitchClass: ks.pitchClass
          , accidental: ks.accidental
          }
      )
    -- retrieve the tonic of what is now a major scale
    Tuple tonic _ = pianoKeyScale pianoKeySignature ks.mode
    -- retain the flat context of the original key (if there is one)
    isFlatCtx = ks.accidental == Flat
    -- translate to a pitch in the new Major key
    (Pitch newKeyPitch) = pianoKeyToPitch isFlatCtx tonic
  in
    { pitchClass: newKeyPitch.pitchClass
    , accidental: newKeyPitch.accidental
    , mode: Major
    }

-- | Transpose a key signature by a given distance.
transposeKeySignatureBy :: Int -> ModifiedKeySignature -> ModifiedKeySignature
transposeKeySignatureBy interval mks =
  let
    -- convert key sig to a piano key
    pianoKey = buildPianoKey
      ( Pitch
          { pitchClass: mks.keySignature.pitchClass
          , accidental: mks.keySignature.accidental
          }
      )

    -- retain the sharp/flat/natural context
    isFlatCtx = mks.keySignature.accidental == Flat
    -- find the position in the octave of this key
    keyPos = fromMaybe 0 $ elemIndex pianoKey pianoOctave
    -- find its new position after moving
    -- be careful to use only values 0 <= n < 12
    newPos = boundedIndex (keyPos + interval)
    -- look up the new piano key
    newPianoKey = fromMaybe (White C) $ index pianoOctave newPos
    -- and convert back (via a pitch)
    Pitch pitch = pianoKeyToPitch isFlatCtx newPianoKey
    newks =
      { pitchClass: pitch.pitchClass
      , accidental: pitch.accidental
      , mode: mks.keySignature.mode
      }
    -- and also transpose any mods to the key signature
    modifications =
      map (transposeKeyAccidentalBy interval) mks.modifications
  in
    { keySignature: newks, modifications: modifications, properties: mks.properties }

-- | a relationship between a Pitch and a note number
-- | i.e. C is 0, C Sharp is 1 B is 11 etc.
-- | Note that B# and B# go above the 12 notes in the scale 
-- | because they effectively jump octave
pitchNumbers :: List (Tuple Pitch Int)
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
      : Tuple (Pitch { pitchClass: B, accidental: Sharp }) 12
      : Tuple (Pitch { pitchClass: B, accidental: DoubleSharp }) 13
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
          Pitch { pitchClass: p.pitchClass, accidental: Natural }
        _ ->
          Pitch p
  in
    fromMaybe 0 $ lookup target chromaticScaleMap

-- IMPLEMENTATION

-- transpose a pitch that defines a key signature modification be the required amount
transposeKeyAccidentalBy :: Int -> Pitch -> Pitch
transposeKeyAccidentalBy interval (Pitch p) =
  let
    -- convert the pitch to a piano key
    pianoKey = buildPianoKey (Pitch p)
    isFlatCtx = p.accidental == Flat
    -- find the position in the octave of this key
    keyPos = fromMaybe 0 $ elemIndex pianoKey pianoOctave
    -- find its new position after moving
    -- be careful to use only values 0 <= n < 12
    newPos = boundedIndex (keyPos + interval)
    newPianoKey = fromMaybe (White C) $ index pianoOctave newPos
  in
    pianoKeyToPitch isFlatCtx newPianoKey

-- the set of black notes determined by the key signature
-- of a diatonic scale (e.g. White C Major means C Major,
-- Black D E Dorian means both C# Dorian or Db Dorian) #
-- (assuming equal temperament)
blackKeySet :: PianoKey -> Mode -> Tuple PianoKey (Array PianoKey)
blackKeySet keySig mode =
  let
    Tuple tonic fullScale = pianoKeyScale keySig mode

    -- filter only the black notes
    isBlackKey :: PianoKey -> Boolean
    isBlackKey (White _) = false
    isBlackKey (Black _ _) = true
  in
    Tuple tonic $ filter isBlackKey fullScale

-- | a complete diatonic scale in terms of PianoKeys governed by the
-- | Piano Key and Mode that defines the key signature
-- | coupled with the tonic for that scale
pianoKeyScale :: PianoKey -> Mode -> Tuple PianoKey (Array PianoKey)
pianoKeyScale keySig mode =
  let
    -- calculate a rotation from C Major taking into account both the
    -- key signature and the mode
    shift = (distanceFromC keySig + distanceFromMajor mode) `mod` notesInChromaticScale
    -- rotate the piano octave by this distance
    -- at this point we have a major scale with major key as the tonic (first note)
    scale = rotate shift pianoOctave
    -- establish the tonic
    tonic = fromMaybe (White C) $ head scale
    -- lookup each position in this new diatonic scale
    lookup key = fromMaybe (White C) $ index scale key
    -- and map each of the diatonic scale offsets to the key found at that position
    keys = map lookup diatonicScaleOffsets
  in
    Tuple tonic keys

-- calculate the number of semitones between the C and the key
distanceFromC :: PianoKey -> Int
distanceFromC keySig =
  fromMaybe 0 $ elemIndex keySig pianoOctave

-- the classical modes are just the major modes shifted a bit
distanceFromMajor :: Mode -> Int
distanceFromMajor mode =
  case mode of
    Dorian -> 10
    Phrygian -> 8
    Lydian -> 7
    Mixolydian -> 5
    Aeolian -> 3
    Minor -> 3
    Locrian -> 1
    Major -> 0
    Ionian -> 0

-- rotate left an array by the specificed amount
rotate :: ∀ a. Int -> Array a -> Array a
rotate n xs = drop n xs <> take n xs

-- Transform the PianoKey into a Pitch with black notes
-- set to Flat or Sharp according to context
pianoKeyToPitch :: Boolean -> PianoKey -> Pitch
pianoKeyToPitch isFlatCtx pianoKey =
  let
    convertPianoKey :: Boolean -> PianoKey -> Pitch
    convertPianoKey _ (White p) =
      Pitch
        { pitchClass: p
        -- , accidental : Implicit
        , accidental: Natural
        }
    convertPianoKey flatCtx (Black p q) =
      if flatCtx then
        Pitch
          { pitchClass: q
          , accidental: Flat
          }
      else
        Pitch
          { pitchClass: p
          , accidental: Sharp
          }
  in
    convertPianoKey isFlatCtx pianoKey

-- modify a key set by adding new accidental
-- This is ussed in order to start with a simple key signature
-- and end up with an extended set of keys by taking into account
-- the modifications which add specific sharos or flats
modifyKeySet :: Pitch -> KeySet -> KeySet
modifyKeySet newP ks =
  case newP of
    -- ignore naturals in incomimg key for key signatures
    Pitch { pitchClass: _, accidental: Natural } ->
      ks
    Pitch { pitchClass: pitchClass, accidental: _ } ->
      newP : (L.filter (noMatchPC pitchClass) ks)
  where
  noMatchPC :: PitchClass -> Pitch -> Boolean
  noMatchPC pc (Pitch p) =
    pc /= p.pitchClass

-- the key signature in terms of a PianoKey
buildPianoKey :: Pitch -> PianoKey
buildPianoKey (Pitch p) =
  case p.accidental of
    Flat -> Black (predecessor p.pitchClass) p.pitchClass
    Sharp -> Black p.pitchClass (successor p.pitchClass)
    _ -> White p.pitchClass

-- find the successor to a PitchClass
-- this is safe because all values have a defined successor (in a ring)
successor :: PitchClass -> PitchClass
successor pc =
  unsafePartial $ fromJust $ succ pc

-- find the predecessor to a PitchClass
-- again, this is safe because all values have a defined predecessor (in a ring)
predecessor :: PitchClass -> PitchClass
predecessor pc =
  unsafePartial $ fromJust $ pred pc

-- take an arbitrary integer (positive or negative) and return a position
-- within a single Piano keyboard in the rance 0 <= n < 12
-- i.e. consider that it rotates uniformly in both directons
boundedIndex :: Int -> Int
boundedIndex i =
  let
    newPos = i `mod` notesInChromaticScale
  in
    if (newPos < 0) then
      notesInChromaticScale + newPos
    else
      newPos

-- special case to rename B Natural as C Flat
-- (only for use in correcting Db scales)
renameBNatural :: KeySet -> KeySet
renameBNatural =
  let
    f :: Pitch -> Pitch
    f (Pitch p) =
      if (p.pitchClass == B) && (p.accidental == Natural) then
        Pitch { pitchClass: C, accidental: Flat }
      else
        Pitch p
  in
    map f

-- lookup for providing offsets from C in a chromatic scale
chromaticScaleMap :: Map Pitch Int
chromaticScaleMap =
  fromFoldable pitchNumbers
