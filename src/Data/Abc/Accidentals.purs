module Data.Abc.Accidentals
        ( Accidentals
        , empty
        , add
        , fromKeySet
        , lookup
        , member
        , explicitAccidental
        , implicitAccidental
        ) where

import Prelude ((==), (<<<), map)
import Data.Abc (PitchClass, Accidental(..), KeyAccidental, KeySet)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)

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
    f ka =
        Tuple ka.pitchClass ka.accidental
    tuples = map (f <<< unwrap) ks
  in
    Map.fromFoldable tuples

-- | lookup a pitch class and see of it exists in the Accidentals set
lookup :: PitchClass -> Accidentals -> Maybe Accidental
lookup  =
  Map.lookup

-- | lookup a KeyAccidental and see if it's a member of the Accidentals set
-- |    (i.e. the value of the Accidental matches for the supplied pitch)
member :: KeyAccidental -> Accidentals -> Boolean
member nka accs =
  let
    ka =
      unwrap nka
    macc =
      lookup ka.pitchClass accs
  in
    (Just ka.accidental) == macc

-- | convert an implict Maybe Accidental (used in key signatures)
-- | to an explict accidental (used in scales) where the
-- | explicit form uses Natural
explicitAccidental :: Maybe Accidental -> Accidental
explicitAccidental macc =
   fromMaybe Natural macc

-- | convert an explict accidental (used in scales)
-- | to an implicit one  (Maybe Accidental - used in key signatures)
-- | where the implicit form uses Nothing in place of Natural
implicitAccidental :: Accidental -> Maybe Accidental
implicitAccidental acc =
  case acc of
    Natural ->
      Nothing
    x ->
      Just x
