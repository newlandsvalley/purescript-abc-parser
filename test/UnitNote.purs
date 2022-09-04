module Test.UnitNote (unitNoteSpec) where

import Prelude (Unit, discard)

import Effect.Aff (Aff)
import Data.Abc.UnitNote (defaultUnitNoteLength, getUnitNoteLength)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Rational (Rational, (%))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

assertOkNoteLen :: String -> Rational -> Aff Unit
assertOkNoteLen source target =
  case parse source of
    Right tune ->
      case (getUnitNoteLength tune) of
        Just rat ->
          target `shouldEqual` rat
        _ ->
          fail "no unit note length"
    _ ->
      fail "parse error"

unitNoteSpec :: Spec Unit
unitNoteSpec = do
  describe "unit note" do
    it "gets UnitNoteLen" do
      assertOkNoteLen manyHeaders (1 % 16)
    it " computes a 4/4 unit note" do
      shouldEqual
        (1 % 8)
        (defaultUnitNoteLength { numerator: 4, denominator: 4 })
    it "computes a 2/4 unit note" do
      shouldEqual
        (1 % 16)
        (defaultUnitNoteLength { numerator: 2, denominator: 4})
    it "finds a default unit note when no meter is present" do
      -- 4/4 is the default
      shouldEqual
        (1 % 8)
        (defaultUnitNoteLength { numerator: 4, denominator: 4 })


manyHeaders :: String
manyHeaders =
  "X: 1\r\nT: Skänklåt efter Brittas Hans\r\nR: Skänklåt\r\nZ: Brian O'Connor, 11/7/2016\r\nL: 1/16\r\nO: Bjorsa\r\nM: 4/4\r\nK:Gmaj\r\n| ABC |\r\n"
