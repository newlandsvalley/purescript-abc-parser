module Test.Normaliser (normaliserSpec) where

import Prelude
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Abc.Parser (parse)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Normaliser (normalise)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

assertNormalised :: String -> String -> Aff Unit
assertNormalised s normalised =
  case (parse s) of
    Right tune ->
      normalised `shouldEqual` (fromTune $ normalise tune)

    Left err ->
      fail ("parse failed: " <> (show err))

normaliserSpec :: Spec Unit
normaliserSpec =
  describe "normalisation" do
    it "normalisation leaves other music items intact " do
      assertNormalised "| ABC4 (3CDE z2 |\x0D\n" "| ABC4 (3CDE z2 |\x0D\n"
    it "normalises a chord" do
      assertNormalised "| [AB]4 [F2E2]2 |\x0D\n" "| [A4B4] [F4E4] |\x0D\n"
    it "normalises a broken rhythm between two notes" do
      assertNormalised "| A4>B4 C2<D2 |\x0D\n" "| A6B2 CD3 |\x0D\n"
    it "normalises a broken rhythm between note and a rest" do
      assertNormalised "| A4>z4 C2<z2 |\x0D\n" "| A6z2 Cz3 |\x0D\n"
    it "normalises a broken rhythm between rest and a note" do
      assertNormalised "| z4>E4 z2<F2 |\x0D\n" "| z6E2 zF3 |\x0D\n"
    it "normalises a doubly broken rhythm between two notes" do
      assertNormalised "| A4>>B4 C4<<D4 |\x0D\n" "| A7B CD7 |\x0D\n"
    it "normalises a broken rhythm with small durations" do
      assertNormalised "| A>B C<D |\x0D\n" "| A3/2B/ C/D3/2 |\x0D\n"
     

