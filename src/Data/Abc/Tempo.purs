-- | Conversion functions for Tempo.
module Data.Abc.Tempo
  ( MidiTick
  , AbcTempo
  , defaultTempo
  , defaultAbcTempo
  , getTempoSig
  , getAbcTempo
  , midiTempo
  , beatsPerSecond
  , getBpm
  , setBpm
  , playedNoteDuration
  , standardMidiTick
  , noteTicks
  , chordalNoteTicks
  ) where

import Data.Abc

import Data.Abc.Meter (getDefaultedMeter)
import Data.Abc.Optics (_bpm, _headers, _Tempo)
import Data.Abc.UnitNote (defaultUnitNoteLength, getUnitNoteLength)
import Data.Foldable (foldl)
import Data.Int (round)
import Data.Lens.Fold (firstOf)
import Data.Lens.Setter (set)
import Data.Lens.Traversal (traversed)
import Data.List ((:), List(..), filter, reverse)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, (%), fromInt, toNumber)
import Prelude (($), (+), (/), (*), (<<<))

-- Exposed API

-- | A MIDI tick - used to give a note duration.
type MidiTick =
  Int

-- | a standard beat is a quarter note
-- | in tempo signatures such as 1/4=120
standardBeatLength :: Rational
standardBeatLength = (1 % 4)

-- | ditto for a standard beat per minute (BPM)
standardBPM :: Int
standardBPM = 120

-- | The tempo when the tune is being played. This is usually represented
-- | as (for example) 1/4 = 120 - i.e. 120 querter notes per minute.
-- | this is a consolidation of both the Tempo and the Unit Note length
-- | which thus encapsulates everything you need to calculate the overall
-- | tempo of the tune
-- |
-- | tempoNoteLength - the note length of a tempo definition
-- | bpm - the beats per minute of a tempo Definition
-- | unitNoteLength - the length of a 'unit note' in the ABC definition
type AbcTempo =
  { tempoNoteLength :: Rational
  , bpm :: Int
  , unitNoteLength :: Rational
  }

-- | The default Tempo - 1/4=120.
defaultTempo :: TempoSignature
defaultTempo =
  { noteLengths: singleton standardBeatLength
  , bpm: standardBPM
  , marking: Nothing
  }

-- | default to 1/4=120 with eighth notes as the default note length
-- | this works out that an eighth notes last for 1/4 second
defaultAbcTempo :: AbcTempo
defaultAbcTempo =
  { tempoNoteLength: standardBeatLength
  , bpm: standardBPM
  , unitNoteLength: 1 % 8
  }

-- | Get the raw tempo signature from the tune 
-- | For more flexibility, you should use the _Tempo optic.
getTempoSig :: AbcTune -> Maybe TempoSignature
getTempoSig tune =
  firstOf (_headers <<< traversed <<< _Tempo) tune  

-- | Get the ABC tempo from the tune
-- | This is usually more useful because it incorporates the unit note length
getAbcTempo :: AbcTune -> AbcTempo
getAbcTempo tune =
  let
    tempoSig = fromMaybe defaultTempo $ getTempoSig tune
    meterSig = getDefaultedMeter tune
    unitNoteLength = fromMaybe (defaultUnitNoteLength meterSig) $ getUnitNoteLength tune
  in
    { tempoNoteLength: foldl (+) (fromInt 0) tempoSig.noteLengths
    , bpm: tempoSig.bpm
    , unitNoteLength: unitNoteLength
    }

{-
   midiTempo algorithm is:
   t.bpm beats occupy 1 minute or 60 * 10^6 μsec
   1 bpm beat occupies 60 * 10^6/t.bpm μsec
   but we use a standard beat of 1 unit when writing a note, whereas the bpm measures a tempo note length of
   t.unitNoteLength/t.tempoNoteLength
   i.e.
   1 whole note beat occupies 60 * 10^6/t.bpm * t.unl/t.tnl μsec
-}

-- | The MIDI tempo measured in microseconds per beat.
-- | JMW!!! check
midiTempo :: AbcTempo -> Int
midiTempo t =
  let
    relativeNoteLength =
      t.unitNoteLength / t.tempoNoteLength
  in
    round ((60.0 * 1000000.0 * (toNumber relativeNoteLength)) / (toNumber $ fromInt t.bpm))

-- | calculate the number of beats per second given by an ABC tempo
-- | to give a simple indication of the tempo of the overall melody
-- | note that this is independent of the unit note length
beatsPerSecond :: AbcTempo -> Rational
beatsPerSecond t =
  (t.bpm % 60) * (t.tempoNoteLength / standardBeatLength)

-- | Get the tempo of the tune in beats per minute from the tunes header
-- |    (if it exists) or the default of 120 if it does not.
getBpm :: AbcTune -> Int
getBpm tune =
  case (firstOf (_headers <<< traversed <<< _Tempo <<< _bpm) tune) of
    Just bpm -> bpm
    _ -> defaultTempo.bpm

-- | Change the tempo of the tune by altering the beats per minute (bpm)
-- | in the tune's tempo header (if it exists) or by altering a newly incorporated
-- | default tempo if not.
setBpm :: Int -> AbcTune -> AbcTune
setBpm bpm tune =
  case (firstOf (_headers <<< traversed <<< _Tempo) tune) of
    Just _ ->
      set (_headers <<< traversed <<< _Tempo <<< _bpm) bpm tune
    _ ->
      let
        t = defaultTempo { bpm = bpm }
        newTempoHeader = (Tempo t)
        newHeaders = replaceTempoHeader newTempoHeader tune.headers
      in
        { headers: newHeaders, body: tune.body }

-- Player support

-- | calculate the note duration when it is played (in seconds)
-- | from an ABC note duration and tempo
playedNoteDuration :: AbcTempo -> Rational -> Number
playedNoteDuration abcTempo noteLength =
  let
    bps = beatsPerSecond abcTempo
    beatLength = abcTempo.unitNoteLength / (1 % 4)
  in
    toNumber $ beatLength * noteLength / bps

-- MIDI support

-- | A standard MIDI tick - we use 1/4 note = 480 ticks.
-- | this is known as 'ticks per quarter note' or 'parts per quarter'
-- | in MIDI literature,  480 tends to be standard.
standardMidiTick :: MidiTick
standardMidiTick =
  480

-- | Assume a standard unit note length of 1/4 and a standard number of ticks per unit (1/4) note of 480.
noteTicks :: Rational -> MidiTick
noteTicks n =
  -- (standardMidiTick * (numerator n)) // (denominator n)
  round $ toNumber $ n * (fromInt standardMidiTick)

-- | Find the MIDI duration of a note within a chord in standard ticks
-- |    (1/4 note == 480 ticks)
chordalNoteTicks :: Rational -> Rational -> MidiTick
chordalNoteTicks note chord =
  round $ toNumber $ note * chord * (fromInt standardMidiTick)

-- implementation

-- | replace a tempo header (if it exists)
replaceTempoHeader :: Header -> TuneHeaders -> TuneHeaders
replaceTempoHeader newTempoHeader hs =
  let
    f h =
      case h of
        Tempo _ ->
          false
        _ ->
          true

    newhs =
      filter f hs
  in
    placeHeaderPenultimately newTempoHeader newhs

-- | the last ABC header should always be the key signature so we'll
-- | choose to set the (altered) tempo header as next-to-last.
placeHeaderPenultimately :: Header -> TuneHeaders -> TuneHeaders
placeHeaderPenultimately h hs =
  case reverse hs of
    Nil ->
      (h : Nil)
    x : xs ->
      reverse (x : h : xs)

