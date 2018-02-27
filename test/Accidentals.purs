module Test.Accidentals where

import Prelude (Unit, discard)
import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Abc.KeySignature (modifiedKeySet)
import Data.Abc (Accidental(..), PitchClass(..), KeySignature, Mode(..))
import Data.Abc.Accidentals as Accidentals
import Test.Unit (Test, TestF, suite, test)
import Test.Unit.Assert as Assert

accidentalsSuite :: forall t. Free (TestF t) Unit
accidentalsSuite =
  suite "accidental lookups" do
    test "f in G Major" do
      Assert.equal
          (Just Sharp)
          (Accidentals.implicitInKeySet F
             (modifiedKeySet { keySignature: gMajor, modifications: Nil }))
    test "f in G Major" do
      Assert.equal
        (Nothing)
        (Accidentals.implicitInKeySet F
           (modifiedKeySet { keySignature: cMajor, modifications: Nil }))

-- key signatures
cMajor :: KeySignature
cMajor =
    { pitchClass: C, accidental: Natural, mode: Major }

gMajor :: KeySignature
gMajor =
    { pitchClass: G, accidental: Natural, mode: Major }
