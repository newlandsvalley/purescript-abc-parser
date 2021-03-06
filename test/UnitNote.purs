module Test.UnitNote (unitNoteSuite) where

import Prelude (Unit, discard, ($))
import Control.Monad.Free (Free)

import Data.Abc.UnitNote (defaultUnitNoteLength)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Test.Unit.Assert as Assert
import Data.Rational ((%))

import Test.Unit (TestF, suite, test)

unitNoteSuite :: Free TestF Unit
unitNoteSuite = do
  suite "unit note" do
    test "4/4 unit note" do
      Assert.equal
        (1 % 8)
        (defaultUnitNoteLength (Just $ Tuple 4 4))
    test "2/4 unit note" do
      Assert.equal
        (1 % 16)
        (defaultUnitNoteLength (Just $ Tuple 2 4))
    test "no meter signature unit note" do
      Assert.equal
        (1 % 8)
        (defaultUnitNoteLength Nothing)