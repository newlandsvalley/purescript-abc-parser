module Test.Metadata (metadataSpec) where

-- | test both the Utils and Meter modules

import Prelude (Unit, discard, pure, unit, ($), (<>), (<<<))
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Lens.Fold (toListOf)
import Data.Lens.Traversal (traversed)
import Data.List (List(..), head, length, (:))
import Data.Abc.Parser (parse)
import Data.Abc  ( TimeSignature, BodyPart(..), AbcTune)
import Data.Abc.Utils
import Data.Abc.Meter (getDefaultedMeter)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Optics (_headers, _Title)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

assertOkTitle :: String -> String -> Aff Unit
assertOkTitle source target =
  case parse source of
    Right tune ->
      case (getTitle tune) of
        Just title ->
          target `shouldEqual` title

        _ ->
          fail "no title"
    _ ->
      fail "parse error"

assertAllTitles :: String -> List String -> Aff Unit
assertAllTitles source target =
  case parse source of
    Right tune ->
      let
        titles =
          toListOf (_headers <<< traversed <<< _Title) tune
      in
        target `shouldEqual` titles
    _ ->
      fail "parse error"

assertOkMeter :: String -> TimeSignature -> Aff Unit
assertOkMeter source target =
  case parse source of
    Right tune ->
      let
        meter =
          getDefaultedMeter tune
      in
        target `shouldEqual` meter
    _ ->
      fail "parse error"

{-}
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
-}

assertNoHeader :: forall h. String -> (AbcTune -> Maybe h) -> Aff Unit
assertNoHeader source getf =
  case parse source of
    Right tune ->
      let
        mtitle =
          getf tune
      in
        case mtitle of
          Just _ ->
            fail "no title expected"
          _ ->
            pure unit

    _ ->
      fail "parse error"

assertHeaderCount :: Int -> String -> Aff Unit
assertHeaderCount expectedCount source =
  case parse source of
    Right tune ->
      expectedCount `shouldEqual` (length tune.headers)

    _ ->
      fail "parse error"

assertEmptyScore :: Boolean -> String -> Aff Unit
assertEmptyScore expected source =
  case parse source of
    Right tune ->
      case (head tune.body) of
        Just (Score bars) ->
          expected `shouldEqual` (isEmptyStave bars)
        _ ->
          fail "test has no Score BodyPart"
    _ ->
      fail "parse error"

buildThumbnail :: String -> String
buildThumbnail s =
  case parse s of
    Right tune ->
      fromTune $ thumbnail tune
    _ ->
      "parse error"

buildThumbnailNoRepeats :: String -> String
buildThumbnailNoRepeats s =
  case parse s of
    Right tune ->
      fromTune $ removeRepeatMarkers $ thumbnail tune
    _ ->
      "parse error"

metadataSpec :: Spec Unit
metadataSpec = do
  describe "metadata" do
    headerSpec
    scoreSpec
    thumbnailSpec

headerSpec :: Spec Unit
headerSpec =
  describe "headers" do
    it "gets title" do
      assertOkTitle titledTune "Gamal Reinlender"
    it "gets no title" do
      assertNoHeader keyedTune getTitle
    it "gets first of multiple titles" do
      assertOkTitle doublyTitledTune "Nancy Dawson"
    it "gets all titles" do
      assertAllTitles doublyTitledTune ("Nancy Dawson" : "Piss Upon the Grass" : Nil)
    it "gets multiple headers" do
      assertHeaderCount 8 manyHeaders
    it "gets meter" do
      assertOkMeter manyHeaders { numerator: 4, denominator: 4}

scoreSpec :: Spec Unit
scoreSpec =
  describe "score" do
    it "recognizes empty score" do
      assertEmptyScore true emptyScore
    it "recognizes a non empty score" do
      assertEmptyScore false keyedTune

thumbnailSpec :: Spec Unit
thumbnailSpec =
  describe "thumbnail" do
    it "copes with lead-in bar" do
      augustssonThumbnail `shouldEqual` (buildThumbnail augustsson)
    it "copes without lead-in bar" do
      fastanThumbnail `shouldEqual` (buildThumbnail fastan)
    it "removes the repeat markers" do
      augustssonThumbnailNoRepeats `shouldEqual` (buildThumbnailNoRepeats augustsson)

-- headers in sample ABC tunes
keyedTune :: String
keyedTune =
  "K: FMajor\x0D\n| ABC |\x0D\n"

titledTune :: String
titledTune =
  "T: Gamal Reinlender\x0D\n| ABC |\x0D\n"

doublyTitledTune :: String
doublyTitledTune =
  "T: Nancy Dawson\x0D\nT: Piss Upon the Grass\x0D\n| ABC |\x0D\n"

manyHeaders :: String
manyHeaders =
  "X: 1\r\nT: Skänklåt efter Brittas Hans\r\nR: Skänklåt\r\nZ: Brian O'Connor, 11/7/2016\r\nL: 1/16\r\nO: Bjorsa\r\nM: 4/4\r\nK:Gmaj\r\n| ABC |\r\n"

emptyScore :: String
emptyScore =
  "| @ # | \\r\n|  |\r\n"

augustssonHeaders :: String
augustssonHeaders =
  "X: 1\r\n"
    <> "T: Engelska efter Albert Augustsson\r\n"
    <> "N: From the playing of Albert Augustsson, Bohuslän county.\r\n"
    <> "M: 4/4\r\n"
    <> "R: Engelska\r\n"
    <> "S: Orust\r\n"
    <> "Z: John Watson 24/01/2015\r\n"
    <> "L: 1/8\r\n"
    <> "K: AMajor\r\n"

augustsson :: String
augustsson =
  augustssonHeaders
    <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
    <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
    <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
    <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

augustssonThumbnail :: String
augustssonThumbnail =
  augustssonHeaders
    <> "A>c|: e2f2 efed | c2a2 e3d |\r\n"

augustssonThumbnailNoRepeats :: String
augustssonThumbnailNoRepeats =
  augustssonHeaders
    <> "A>c| e2f2 efed | c2a2 e3d |\r\n"

fastanHeaders :: String
fastanHeaders =
  "T: Fastan\r\n"
    <> "R: Polska\r\n"
    <> "M: 3/4\r\n"
    <> "K: FMajor\r\n"
    <> "L: 1/16\r\n"

fastan :: String
fastan =
  fastanHeaders
    <> "| (3A4F4G4 A2B2 | (3:2:4c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
    <> "| (3A4F4G4 A2B2 | (3:2:4c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
    <> "| (3:2:5F4B4cBA2 B2d2 | ge3 c4 A4- | (3:2:5A4B4cBA2 B2d2 | de3 c8- |\r\n"
    <> "| (3:2:5F4B4cBA2 B2d2 | (3:2:4g2a2f4g4 e4- | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"

fastanThumbnail :: String
fastanThumbnail =
  fastanHeaders
    <> "| (3A4F4G4 A2B2 | (3:2:4c2d2B4c4 A2F2 |\r\n"
