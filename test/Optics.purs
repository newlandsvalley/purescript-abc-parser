module Test.Optics (opticsSuite) where

import Data.Abc
import Data.Abc.Optics

import Control.Monad.Free (Free)
import Data.Abc.Parser (parse)
import Data.Either (hush)
import Data.Lens.Fold (firstOf, toListOf)
import Data.Lens.Setter (set)
import Data.Lens.Traversal (traversed)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Rational ((%))
import Partial.Unsafe (unsafePartial)
import Prelude (($), (<>), (<<<), Unit, discard)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

opticsSuite :: Free TestF Unit
opticsSuite =
  suite "optics" do
    test "title" do
      let
        title :: Maybe String
        title = firstOf (_headers <<< traversed <<< _Title) (getTune borddajnsijn)
      Assert.equal title (Just "Borddajnsijn")
    test "titles" do
      let
        titles :: List String
        titles = toListOf (_headers <<< traversed <<< _Title) (getTune borddajnsijn)
      Assert.equal titles ("Borddajnsijn" : "Second title" : Nil)
    test "unit note length" do
      let
        duration :: Maybe NoteDuration
        duration = firstOf (_headers <<< traversed <<< _UnitNoteLength) (getTune borddajnsijn)
      Assert.equal duration (Just (1 % 8))
    test "mode" do
      let
        mode :: Maybe Mode
        mode = firstOf
          (_headers <<< traversed <<< _ModifiedKeySignature <<< _keySignature <<< _mode)
          (getTune borddajnsijn)
      Assert.equal mode (Just Major)
    test "key pitch class" do
      let
        pitchClass :: Maybe PitchClass
        pitchClass = firstOf
          (_headers <<< traversed <<< _ModifiedKeySignature <<< _keySignature <<< _pitchClass)
          (getTune borddajnsijn)
      Assert.equal pitchClass (Just G)
    test "tempo bpm" do
      let
        bpm :: Maybe Int
        bpm = firstOf
          (_headers <<< traversed <<< _Tempo <<< _bpm)
          (getTune borddajnsijn)
      Assert.equal bpm (Just 150)
    test "voice id" do
      let
        id :: Maybe String
        id = firstOf
          (_headers <<< traversed <<< _Voice <<< _id)
          (getTune borddajnsijn)
      Assert.equal id (Just "1")
    test "set title" do
      let
        newTune = set (_headers <<< traversed <<< _Title) "new title" (getTune borddajnsijn)

        title :: Maybe String
        title = firstOf (_headers <<< traversed <<< _Title) newTune
      Assert.equal title (Just "new title")

getTune :: String -> AbcTune
getTune s =
  -- (unsafePartial <<< fromJust <<< hush <<< parse)
  unsafePartial $ fromJust $ hush $ parse s

borddajnsijn :: String
borddajnsijn =
  "X: 1\r\n"
    <> "T: Borddajnsijn\r\n"
    <> "T: Second title\r\n"
    <> "R: polka\r\n"
    <> "L: 1/8\r\n"
    <> "Q: 1/4=150\r\n"
    <> "K: GMajor\r\n"
    <> "V: 1\r\n"
    <> "|: d2 d2 | d3 B | dc AF | GA Bc | d2 g2 | d3 B | dc AF | G2 z2 :|\r\n"
    <> "|: DF AF | DG Bd | dc AF | GB d2 | DF AF | DG Bd | dc AF | G2 z2 :|\r\n"
    <> "\r\n"
    <> "\r\n"