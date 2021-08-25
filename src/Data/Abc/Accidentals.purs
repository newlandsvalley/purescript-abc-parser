-- | Accidentals are a set of mappings from a PitchClass to an Accidental
-- | They are useful, for example, when interpreting a bar of music where
-- | the accidental nature of a note may depend on the existence of a previous
-- | note of the same pitch within the bar.
module Data.Abc.Accidentals
  ( Accidentals
  , empty
  , add
  , fromKeySet
  , lookup
  , member
  , fromKeySig
  , implicitInKeySet
  ) where

import Prelude ((==), map)
import Data.Abc (PitchClass, Accidental, Pitch(..), KeySet, KeySignature)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))

-- | A set of accidentals
type Accidentals =
  Map.Map PitchClass Accidental

-- | create an empty set of Key Accidentals
empty :: Accidentals
empty =
  Map.empty

-- | add an accidental to the set
add :: PitchClass -> Accidental -> Accidentals -> Accidentals
add pc acc accs =
  Map.insert pc acc accs

-- | build Accidentals from a KeySet
fromKeySet :: KeySet -> Accidentals
fromKeySet ks =
  let
    f kar =
      case kar of
        Pitch p ->
          Tuple p.pitchClass p.accidental
    tuples = map f ks
  in
    Map.fromFoldable tuples

-- | lookup a pitch class and see of it exists in the Accidentals set
lookup :: PitchClass -> Accidentals -> Maybe Accidental
lookup =
  Map.lookup

-- | lookup a KeyAccidental (represented as a Pitch) and see if it's a member of
-- | the Accidentals set  (i.e. the value of the Accidental matches for the supplied pitch)
member :: Pitch -> Accidentals -> Boolean
member (Pitch p) accs =
  let
    macc =
      lookup p.pitchClass accs
  in
    (Just p.accidental) == macc

-- | extract the KeyAccidental Pitch from a KeySignature
fromKeySig :: KeySignature -> Pitch
fromKeySig ks =
  Pitch
    { pitchClass: ks.pitchClass
    , accidental: ks.accidental
    }

-- | Return an accidental if it is implicitly there in the supplied KeySet
-- | (which is obtained from a key signature)
-- | attached to the pitch class of the note. In ABC, notes generally inherit
-- | their (sharp, flat or natural) accidental nature from the key signature.
implicitInKeySet :: PitchClass -> KeySet -> Maybe Accidental
implicitInKeySet pc keyset =
  lookup pc (fromKeySet keyset)
