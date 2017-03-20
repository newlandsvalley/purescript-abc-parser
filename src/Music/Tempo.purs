module Music.Tempo
        ( defaultTempo
        , getBpm
        , setBpm
        ) where

import Prelude (($))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..), (:), filter, reverse)
import Data.Rational (rational)
import Abc.ParseTree
import Music.Notation (getHeader)

-- Exposed API

-- | The default Tempo - 1/4=120.
defaultTempo :: TempoSignature
defaultTempo =
    { noteLengths: ( rational 1 4 : Nil)
    , bpm: 120
    , marking: Nothing
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

{-}
  let
    headerMap =
      getHeaderMap tune
  in
    fromMaybe (Tempo defaultTempo) $ lookup 'Q' headerMap
-}

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
