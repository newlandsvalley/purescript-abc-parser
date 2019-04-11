module Test.Voice (voiceSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Parser (parse)
import Data.Abc.Voice (partitionTuneBody)
import Data.Array (index, length)
import Data.Maybe (fromMaybe)
import Data.List (List(..))
import Data.Either (Either(..))
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

assertVoiceCount :: String -> Int -> Test
assertVoiceCount s target =
  case (parse s) of
    Right tune ->
      Assert.equal target (length (partitionTuneBody tune.body))

    Left err ->
      failure ("parse failed: " <> (show err))

assertVoice :: String -> String -> Int -> Test
assertVoice s canonical ix =
  case (parse s) of
    Right tune ->
      let
        partitionedBody = partitionTuneBody tune.body
        firstBody = fromMaybe Nil $ index partitionedBody ix
        firstVoice = { headers: tune.headers, body: firstBody }
      in
        Assert.equal canonical (fromTune firstVoice)

    Left err ->
      failure ("parse failed: " <> (show err))

voiceSuite :: Free TestF Unit
voiceSuite = do
  suite "voice" do
    test "no voices" do
      assertVoiceCount noVoice 1
    test "one voice" do
      assertVoiceCount oneVoice 1
    test "two voices" do
      assertVoiceCount twoVoices 2
    test "three voices" do
      assertVoiceCount threeVoices 3
    test "first voice of two" do
      assertVoice twoVoices firstVoiceOfTwo 0
    test "second voice of two" do
      assertVoice twoVoices secondVoiceOfTwo 1
    test "three voices with empty stave" do
      assertVoiceCount (threeVoices <> "\x0D\n") 3

noVoice :: String
noVoice =
    "K: CMajor\x0D\n| AB (3zde [fg] |\x0D\n| CD EF FG |\x0D\n| AB EF FG |\x0D\n"

oneVoice :: String
oneVoice =
    "K: CMajor\x0D\n[V:T1]| AB (3zde [fg] |\x0D\n[V:T1]| CD EF FG |\x0D\n[V:T1]| AB EF FG |\x0D\n"

twoVoices :: String
twoVoices =
    "K: CMajor\x0D\n[V:T1]| AB (3zde [fg] |\x0D\n[V:T2]| CD EF FG |\x0D\n" <>
    "[V:T1]| AB EF FG |\x0D\n[V:T2]| AB (3zde [fg] |\x0D\n"

threeVoices :: String
threeVoices =
    "K: CMajor\x0D\n[V:T1]| AB (3zde [fg] |\x0D\n[V:T2]| CD EF FG |\x0D\n" <>
    "[V:T1]| AB EF FG |\x0D\n[V:T3]| AB (3zde [fg] |\x0D\n"

-- the first voice of the twoVoices
firstVoiceOfTwo :: String
firstVoiceOfTwo =
    "K: CMajor\x0D\n[V: T1]| AB (3zde [fg] |\x0D\n[V: T1]| AB EF FG |\x0D\n"

-- the second voice of the twoVoices
secondVoiceOfTwo :: String
secondVoiceOfTwo =
    "K: CMajor\x0D\n[V: T2]| CD EF FG |\x0D\n[V: T2]| AB (3zde [fg] |\x0D\n"
