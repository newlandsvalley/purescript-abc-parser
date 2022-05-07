module Test.Optics (opticsSpec) where

import Data.Abc
import Data.Abc.Optics

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
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

opticsSpec :: Spec Unit
opticsSpec =
  describe "optics" do
    it "fetches the title" do
      let
        title :: Maybe String
        title = firstOf (_headers <<< traversed <<< _Title) (getTune borddajnsijn)
      title `shouldEqual` (Just "Borddajnsijn")
    it "fetches all titles" do
      let
        titles :: List String
        titles = toListOf (_headers <<< traversed <<< _Title) (getTune borddajnsijn)
      titles `shouldEqual` ("Borddajnsijn" : "Second title" : Nil)
    it "fetches unit note length" do
      let
        duration :: Maybe NoteDuration
        duration = firstOf (_headers <<< traversed <<< _UnitNoteLength) (getTune borddajnsijn)
      duration `shouldEqual` (Just (1 % 8))
    it "fetches the mode" do 
      let
        mode :: Maybe Mode
        mode = firstOf
          (_headers <<< traversed <<< _ModifiedKeySignature <<< _keySignature <<< _mode)
          (getTune borddajnsijn)
      mode `shouldEqual` (Just Major)
    it "fetches the key pitch class" do
      let
        pitchClass :: Maybe PitchClass
        pitchClass = firstOf
          (_headers <<< traversed <<< _ModifiedKeySignature <<< _keySignature <<< _pitchClass)
          (getTune borddajnsijn)
      pitchClass `shouldEqual` (Just G)
    it "fetches the tempo bpm" do
      let
        bpm :: Maybe Int
        bpm = firstOf
          (_headers <<< traversed <<< _Tempo <<< _bpm)
          (getTune borddajnsijn)
      bpm `shouldEqual` (Just 150)
    it "fetches the voice id" do
      let
        id :: Maybe String
        id = firstOf
          (_headers <<< traversed <<< _Voice <<< _id)
          (getTune borddajnsijn)
      id `shouldEqual` (Just "1")
    it "sets the  title" do
      let
        newTune = set (_headers <<< traversed <<< _Title) "new title" (getTune borddajnsijn)

        title :: Maybe String
        title = firstOf (_headers <<< traversed <<< _Title) newTune
      title `shouldEqual` (Just "new title")

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