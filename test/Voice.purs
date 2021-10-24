module Test.Voice (voiceSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Abc (AbcTune)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Metadata (getTitle)
import Data.Abc.Parser (parse)
import Data.Abc.Voice (getVoiceLabels, getVoiceMap, partitionTuneBody, partitionVoices)
import Data.Array (index, length)
import Data.Map (keys, size, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..))
import Data.List (fromFoldable, length) as List
import Data.Either (Either(..))
import Data.Set (Set)
import Data.Set (fromFoldable) as Set
import Data.Unfoldable (replicate)
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

assertVoiceCount :: String -> Int -> Test
assertVoiceCount s target =
  case (parse s) of
    Right tune ->
      Assert.equal target (length (partitionTuneBody tune))

    Left err ->
      failure ("parse failed: " <> (show err))

-- | assert that a canonical tune is equal to the partitioned tune 
-- | in the array of voice-partitioned tunes at the stated index
assertVoice :: String -> String -> Int -> Test
assertVoice s canonical ix =
  case (parse s) of
    Right tune ->
      let
        partitionedBody = partitionTuneBody tune
        indexedBody = fromMaybe Nil $ index partitionedBody ix
        indexedVoice = { headers: tune.headers, body: indexedBody }
      in
        Assert.equal canonical (fromTune indexedVoice)

    Left err ->
      failure ("parse failed: " <> (show err))

-- | ditto but use partitionVoices instead of partitionTuneBody
assertVoice' :: String -> String -> Int -> Test
assertVoice' s canonical ix =
  case (parse s) of
    Right tune ->
      let
        partitionedVoices :: Array AbcTune
        partitionedVoices = partitionVoices tune

        indexedVoice :: Maybe AbcTune
        indexedVoice = index partitionedVoices ix
      in
        Assert.equal (Just canonical) (map fromTune indexedVoice)

    Left err ->
      failure ("parse failed: " <> (show err))

-- assert the set of voice names found in the tune after asking for just the labels
assertVoiceLabels :: String -> Array String -> Test
assertVoiceLabels s target =
  case (parse s) of
    Right tune ->
      Assert.equal target (getVoiceLabels tune)

    Left err ->
      failure ("parse failed: " <> (show err))

-- ditto after asking for the entire voice map
assertVoiceMapLabels :: String -> Set String -> Test
assertVoiceMapLabels s target =
  case (parse s) of
    Right tune ->
      let
        voiceMap = getVoiceMap tune
      in
        Assert.equal target (keys voiceMap)

    Left err ->
      failure ("parse failed: " <> (show err))

-- check that the partitioned voice has a title representing the voice name
-- when using getVoiceMap
assertVoiceTitles :: String -> List (Maybe String) -> Test
assertVoiceTitles s target =
  case (parse s) of
    Right tune ->
      let
        voiceMap = getVoiceMap tune
        titles = map getTitle (values voiceMap)
      in
        Assert.equal target titles

    Left err ->
      failure ("parse failed: " <> (show err))

-- check that the partitioned voice has a title representing the voice name
-- when using partitionVoices
assertPartitionedVoiceTitles :: String -> Array (Maybe String) -> Test
assertPartitionedVoiceTitles s target =
  case (parse s) of
    Right tune ->
      let
        voices = partitionVoices tune
        titles = map getTitle voices
      in
        Assert.equal target titles

    Left err ->
      failure ("parse failed: " <> (show err))

assertRetitlingPreservesHeaders :: String -> Test
assertRetitlingPreservesHeaders s =
  case (parse s) of
    Right tune ->
      let
        tuneHeaderLength = List.length (tune.headers)
        voiceMap = getVoiceMap tune
        voiceHeaderLengths = map (List.length <<< _.headers) (values voiceMap)
      in
        Assert.equal (replicate (size voiceMap) tuneHeaderLength) voiceHeaderLengths

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
    test "four voices" do
      assertVoiceCount fourVoices 4
    test "two voices inline" do
      assertVoiceCount twoVoicesInline 2
    test "three voices" do
      assertVoiceCount threeVoices 3
    test "first voice of two" do
      assertVoice twoVoices firstVoiceOfTwo 0
    test "first voice of two inline" do
      assertVoice twoVoicesInline firstVoiceOfTwoInline 0
    test "second voice of two" do
      assertVoice' twoVoices secondVoiceOfTwo 1
    test "second voice of two inline" do
      assertVoice' twoVoicesInline secondVoiceOfTwoInline 1
    test "fourth voice of four" do
      assertVoice' fourVoices fourthVoiceOfFour 3
    test "three voices with empty stave" do
      assertVoiceCount (threeVoices <> "\x0D\n") 3
    test "labels - no voice" do
      assertVoiceLabels noVoice []
    test "labels - one voice" do
      assertVoiceLabels oneVoice [ "T1" ]
    test "labels - two voice inline" do
      assertVoiceLabels twoVoicesInline [ "T1", "T2" ]
    test "labels - four voices" do
      assertVoiceLabels fourVoices [ "1", "2", "3", "4" ]
    test "labels from voice map - four voices" do
      assertVoiceMapLabels fourVoices (Set.fromFoldable [ "1", "2", "3", "4" ])
    test "retitling the voice header via getVoiceMap - two voices" do
      assertVoiceTitles twoVoicesTitled (List.fromFoldable [ Just "voice T1", Just "voice T2" ])
    test "retitling the voice header via getVoiceMap - three voices" do
      assertVoiceTitles threeVoices (List.fromFoldable [ Just "voice T1", Just "voice T2", Just "voice T3" ])
    test "retitling preserves other headers" do
      assertRetitlingPreservesHeaders threeVoices
    test "retitling the voice header via partitionVoices - two voices" do
      assertPartitionedVoiceTitles twoVoicesTitled ([ Just "voice T1", Just "voice T2" ])

noVoice :: String
noVoice =
  "K: CMajor\x0D\n| AB (3zde [fg] |\x0D\n| CD EF FG |\x0D\n| AB EF FG |\x0D\n"

oneVoice :: String
oneVoice =
  "X: 1\x0D\nT: One Voice\x0D\n" <>
    "K: CMajor\x0D\n[V:T1]| AB (3zde [fg] |\x0D\n[V:T1]| CD EF FG |\x0D\n[V:T1]| AB EF FG |\x0D\n"

twoVoicesInline :: String
twoVoicesInline =
  "K: CMajor\x0D\n[V:T1]| AB (3zde [fg] |\x0D\n[V:T2]| CD EF FG |\x0D\n" <>
    "[V:T1]| AB EF FG |\x0D\n[V:T2]| AB (3zde [fg] |\x0D\n"

twoVoices :: String
twoVoices =
  "K: CMajor\x0D\nV:T1\r\n| AB (3zde [fg] |\x0D\n| AB EF FG |\x0D\nV:T2\r\n" <>
    "| CD EF FG |\x0D\n| AB (3zde [fg] |\x0D\n"

twoVoicesTitled :: String
twoVoicesTitled =
  "T: two voices\r\nK: CMajor\x0D\nV:T1\r\n| AB (3zde [fg] |\x0D\n| AB EF FG |\x0D\nV:T2\r\n" <>
    "| CD EF FG |\x0D\n| AB (3zde [fg] |\x0D\n"

threeVoices :: String
threeVoices =
  "X: 1\x0D\nT: Three Voices\x0D\n"
    <> "K: CMajor\x0D\n[V:T1]| AB (3zde [fg] |\x0D\n[V:T2]| CD EF FG |\x0D\n"
    <>
      "[V:T1]| AB EF FG |\x0D\n[V:T3]| AB (3zde [fg] |\x0D\n"

-- the first voice of the twoVoices
firstVoiceOfTwo :: String
firstVoiceOfTwo =
  "K: CMajor\x0D\nV: T1\r\n| AB (3zde [fg] |\r\n| AB EF FG |\x0D\n"

-- the first voice of the twoVoices (inline representation)
firstVoiceOfTwoInline :: String
firstVoiceOfTwoInline =
  "K: CMajor\x0D\n[V: T1]| AB (3zde [fg] |\x0D\n[V: T1]| AB EF FG |\x0D\n"

-- the second voice of the twoVoices
secondVoiceOfTwo :: String
secondVoiceOfTwo =
  "X: 1\r\nT: voice T2\r\nK: CMajor\x0D\nV: T2\r\n| CD EF FG |\x0D\n| AB (3zde [fg] |\x0D\n"

-- the second voice of the twoVoices (inline representation)
secondVoiceOfTwoInline :: String
secondVoiceOfTwoInline =
  "X: 1\r\nT: voice T2\r\nK: CMajor\x0D\n[V: T2]| CD EF FG |\x0D\n[V: T2]| AB (3zde [fg] |\x0D\n"

-- Modified four Voice example (from abcnotation.com)
-- added the foo=bar property for a voice header to prepare for v2.2
-- we use 4/4 meter instead of C, CMajor instead of C and spaces after 
-- header colons to make it easier to test against the canonical form
fourVoices :: String
fourVoices =
  "X: 1\r\n"
    <> "T: Grand Staff With Four Voices\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/2\r\n"
    <> "K: CMajor\r\n"
    <> "V: 1 clef=treble foo=bar\r\n"
    <> "c/e/d/c/|c/B/B/c/|c2|]\r\n"
    <> "V: 2 clef=treble\r\n"
    <> "EF|ED|E2|]\r\n"
    <> "V: 3 clef=bass\r\n"
    <> "G,A,|G,G,|G,2|]\r\n"
    <> "V: 4 clef=bass\r\n"
    <> "C,F,|G,G,,|C,2|]\r\n"

{- This is the actual example and seems awkward and uses
   a variety of volatile features which will be regularized
   to some extent in release 2.2
fourVoices :: String 
fourVoices =
    "X:1\r\n" <>
    "T:Grand Staff With Four Voices\r\n" <>
    "M:C\r\n" <>
    "L:1/2\r\n" <>
    "K:\r\n" <>
    "%%staves {1 2 3 4}\r\n" <>
    "K:C\r\n" <>
    "V:1 [K:clef=treble]\r\n" <>
    "c/e/d/c/|c/B/B/c/|c2|]\r\n" <>
    "V:2 [K:clef=treble]\r\n" <>
    "EF|ED|E2|]\r\n" <>
    "V:3 [K:clef=bass]\r\n" <>
    "G,A,|G,G,|G,2|]\r\n" <>
    "V:4 [K:clef=bass]\r\n" <>
    "C,F,|G,G,,|C,2|]\r\n" 
-}

fourthVoiceOfFour :: String
fourthVoiceOfFour =
  "X: 1\r\n"
    <> "T: voice 4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/2\r\n"
    <> "K: CMajor\r\n"
    <> "V: 4 clef=bass\r\n"
    <> "C,F,|G,G,,|C,2|]\r\n"
