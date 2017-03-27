module Data.Abc.Tempo
        ( defaultTempo
        , defaultAbcTempo
        , getAbcTempo
        , getBpm
        , setBpm
        ) where

import Prelude (($), (+))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..), (:), filter, reverse)
import Data.Foldable (foldl)
import Data.Rational (rational, fromInt)
import Data.Abc
import Data.Abc.Notation (AbcTempo, getUnitNoteLength, getTempoSig, getHeader)

-- Exposed API

-- | The default Tempo - 1/4=120.
defaultTempo :: TempoSignature
defaultTempo =
    { noteLengths: ( rational 1 4 : Nil)
    , bpm: 120
    , marking: Nothing
    }

-- | default to 1/4=120
defaultAbcTempo :: AbcTempo
defaultAbcTempo =
    { tempoNoteLength : rational 1 4
    , bpm : 120
    , unitNoteLength : rational 1 8
    }

-- | Get the ABC tempo from the tune
getAbcTempo :: AbcTune -> AbcTempo
getAbcTempo tune =
  let
    tempoSig = fromMaybe defaultTempo $ getTempoSig tune
    unitNoteLength = fromMaybe (rational 1 8) $ getUnitNoteLength tune
  in
    { tempoNoteLength : foldl (+) (fromInt 0) tempoSig.noteLengths
    , bpm : tempoSig.bpm
    , unitNoteLength : unitNoteLength
    }

-- | Get the tempo of the tune in beats per minute from the tunes header
-- |    (if it exists) or the default of 120 if it does not.
getBpm :: AbcTune -> Int
getBpm tune =
  case (tempoHeader tune) of
    Tempo t ->
      t.bpm

    _ ->
      defaultTempo.bpm

-- | Change the tempo of the tune by altering the beats per minute (bpm)
-- | in the tune's tempo header (if it exists) or by altering a newly incorporated
-- | default tempo if not.
setBpm :: Int -> AbcTune -> AbcTune
setBpm bpm tune =
  let
    newTempoHeader =
      case (tempoHeader tune) of
        Tempo t ->
          Tempo t { bpm = bpm }

        -- can't happen but type-checker can't know
        x ->
          x

    newHeaders =
      replaceTempoHeader newTempoHeader tune.headers
  in
    { headers: newHeaders, body: tune.body }


-- implementation
-- | get the tempo header
tempoHeader :: AbcTune -> Header
tempoHeader tune =
  fromMaybe (Tempo defaultTempo) $  getHeader 'Q' tune

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
