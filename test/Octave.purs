module Test.Octave (octaveSuite) where

import Prelude (Unit, bind, discard)
import Control.Monad.Free (Free)

import Data.Abc.Octave (down, up)
import Test.Utils

import Test.Unit (TestF, suite, test)

octaveSuite :: forall t. Free (TestF t) Unit
octaveSuite = do
  suite "octave" do
    test "phrase 1 low to med" do
      assertMoveMatches
        phrase1Low
        up
        phrase1Med
    test "phrase 1 med to high" do
      assertMoveMatches
        phrase1Med
        up
        phrase1High
    test "phrase 1 med to low" do
      assertMoveMatches
        phrase1Med
        down
        phrase1Low
    test "phrase 1 high to Med" do
      assertMoveMatches
        phrase1High
        down
        phrase1Med
    test "phrase 2 low to med" do
      assertMoveMatches
        phrase2Low
        up
        phrase2Med
    test "phrase 2 med to high" do
      assertMoveMatches
        phrase2Med
        up
        phrase2High
    test "phrase 2 med to low" do
      assertMoveMatches
        phrase2Med
        down
        phrase2Low
    test "phrase 2 high to Med" do
      assertMoveMatches
        phrase2High
        down
        phrase2Med

phrase1Low =
    "K: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"

phrase1Med =
    "K: CMajor\x0D\n| AB (3cde [fg] |\x0D\n"

phrase1High =
    "K: CMajor\x0D\n| ab (3c'd'e' [f'g'] |\x0D\n"

phrase2Low =
    "| A,,,B,,, C,D,E, [fg] |\x0D\n| a'>b' |\x0D\n"

phrase2Med =
    "| A,,B,, CDE [f'g'] |\x0D\n| a''>b'' |\x0D\n"

phrase2High =
    "| A,B, cde [f''g''] |\x0D\n| a'''>b''' |\x0D\n"
