module Test.Accidentals (accidentalsSpec) where

import Prelude (Unit, discard)
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Map (empty)
import Data.Abc.KeySignature (modifiedKeySet)
import Data.Abc (Accidental(..), PitchClass(..), KeySignature, Mode(..))
import Data.Abc.Accidentals as Accidentals
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


accidentalsSpec :: Spec Unit
accidentalsSpec =
  describe "accidental lookups" do
    it "looks up f in G Major" do
      shouldEqual
        (Just Sharp)
        ( Accidentals.implicitInKeySet F
            (modifiedKeySet { keySignature: gMajor, modifications: Nil, properties: empty })
        )
    it "looks up f in G Major" do
      shouldEqual
        (Nothing)
        ( Accidentals.implicitInKeySet F
            (modifiedKeySet { keySignature: cMajor, modifications: Nil, properties: empty })
        )

-- key signatures
cMajor :: KeySignature
cMajor =
  { pitchClass: C, accidental: Natural, mode: Major }

gMajor :: KeySignature
gMajor =
  { pitchClass: G, accidental: Natural, mode: Major }
