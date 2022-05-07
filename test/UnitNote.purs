module Test.UnitNote (unitNoteSpec) where

import Prelude (Unit, discard)

import Data.Abc.UnitNote (defaultUnitNoteLength)
import Data.Tuple (Tuple(..))
-- import Test.Unit.Assert as Assert
import Data.Rational ((%))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

unitNoteSpec :: Spec Unit
unitNoteSpec = do
  describe "unit note" do
    it " computes a 4/4 unit note" do
      shouldEqual
        (1 % 8)
        (defaultUnitNoteLength (Tuple 4 4))
    it "computes a 2/4 unit note" do
      shouldEqual
        (1 % 16)
        (defaultUnitNoteLength (Tuple 2 4))
    it "finds a default unit note when no meter is present" do
      -- 4/4 is the default
      shouldEqual
        (1 % 8)
        (defaultUnitNoteLength (Tuple 4 4))