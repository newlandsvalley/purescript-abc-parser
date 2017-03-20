module Data.Abc.Accidentals
        ( Accidentals
        , empty
        , add
        , fromKeySet
        , lookup
        , member
        ) where

import Prelude ((==), (<<<), map)
import Data.Abc (PitchClass, Accidental, KeyAccidental, KeySet)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)

{-| A set of accidentals,  String is a proxy for PitchClass
-}
type Accidentals =
    Map.Map PitchClass Accidental


{-| create an empty set of Key Accidentals
-}
empty :: Accidentals
empty =
    Map.empty


{-| add an accidental to the set
-}
add :: PitchClass -> Accidental -> Accidentals -> Accidentals
add pc acc accs =
    Map.insert pc acc accs


{-| build Accidentals from a KeySet
-}
fromKeySet :: KeySet -> Accidentals
fromKeySet ks =
  let
    f ka =
        Tuple ka.pitchClass ka.accidental
    tuples = map (f <<< unwrap) ks
  in
    Map.fromFoldable tuples

{-
fromKeySet ks =
  let
    tuples = map (\k -> Tuple k.pitchClass k.accidental) ks
  in
    Map.fromFoldable tuples
    -}



{-| lookup a pitch class and see of it exists in the Accidentals set
-}
lookup :: PitchClass -> Accidentals -> Maybe Accidental
lookup  =
    Map.lookup


{-| lookup a KeyAccidental and see if it's a member of the Accidentals set
    (i.e. the value of the Accidental matches for the supplied pitch)
-}
member :: KeyAccidental -> Accidentals -> Boolean
member nka accs =
  let
    ka =
      unwrap nka
    macc =
      lookup ka.pitchClass accs
  in
    (Just ka.accidental) == macc
