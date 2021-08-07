module Test.Accidentals where

import Prelude (Unit, discard)
import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Map (empty)
import Data.Abc.KeySignature (modifiedKeySet)
import Data.Abc (Accidental(..), PitchClass(..), KeySignature, Mode(..))
import Data.Abc.Accidentals as Accidentals
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

accidentalsSuite :: Free TestF Unit
accidentalsSuite =
  suite "accidental lookups" do
    test "f in G Major" do
      Assert.equal
          (Just Sharp)
          (Accidentals.implicitInKeySet F
             (modifiedKeySet { keySignature: gMajor, modifications: Nil, properties: empty }))
    test "f in G Major" do
      Assert.equal
        (Nothing)
        (Accidentals.implicitInKeySet F
           (modifiedKeySet { keySignature: cMajor, modifications: Nil, properties: empty }))

-- key signatures
cMajor :: KeySignature
cMajor =
    { pitchClass: C, accidental: Natural, mode: Major }

gMajor :: KeySignature
gMajor =
    { pitchClass: G, accidental: Natural, mode: Major }
