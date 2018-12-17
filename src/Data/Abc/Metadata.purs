-- | A Ragbag of convenience functions for getting Metadata from ABC headers
module Data.Abc.Metadata
        ( HeaderMap
        , getKeySet
        , getHeader
        , getKeySig
        , getMeter
        , getTempoSig
        , getTitle
        , getUnitNoteLength
        , dotFactor
        , isEmptyStave
        ) where

import Data.Abc
import Data.List (List(..), null, reverse)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, (%))
import Data.Tuple (Tuple(..))
import Data.Foldable (all)
import Data.Abc.KeySignature (modifiedKeySet)
import Prelude (map, ($), (||))

-- | A representation of the ABC headers as a Map, taking the first definition
-- | of any header if multiple definitions are present in the ABC.
type HeaderMap =
    Map Char Header

-- EXPORTED FUNCTIONS

-- | Get the set of key accidentals from the (possibly modified) key (if there is one in the tune).
getKeySet :: AbcTune -> KeySet
getKeySet t =
  let
    mksig =
      getKeySig t
  in
    case mksig of
      Just ksig ->
        modifiedKeySet ksig
      Nothing ->
        Nil

-- | A map (Header code => Header) for the first instance of each Header
getHeaderMap :: AbcTune -> HeaderMap
getHeaderMap t =
  let
    f :: Header -> Tuple Char Header
    f h =
      case h of
        Area _ ->
          Tuple 'A' h

        Book _ ->
          Tuple 'B' h

        Composer _ ->
          Tuple 'C' h

        Discography _ ->
          Tuple 'D' h

        FileUrl _ ->
          Tuple 'F' h

        Group _ ->
          Tuple 'G' h

        History _ ->
          Tuple 'H' h

        Instruction _ ->
          Tuple 'I' h

        Key _ ->
          Tuple 'K' h

        UnitNoteLength _ ->
          Tuple 'L' h

        Meter _ ->
          Tuple 'M' h

        Macro _ ->
          Tuple 'm' h

        Notes _ ->
          Tuple 'N' h

        Origin _ ->
          Tuple 'O' h

        Parts _ ->
          Tuple 'P' h

        Tempo _ ->
          Tuple 'Q' h

        Rhythm _ ->
          Tuple 'R' h

        Remark _ ->
          Tuple 'r' h

        Source _ ->
          Tuple  'S' h

        SymbolLine _ ->
          Tuple 's' h

        Title _ ->
          Tuple 'T' h

        UserDefined _ ->
          Tuple 'U' h

        Voice _ ->
          Tuple 'V' h

        WordsAfter _ ->
          Tuple 'W' h

        WordsAligned _ ->
          Tuple  'w' h

        ReferenceNumber _ ->
          Tuple 'X' h

        Transcription _ ->
          Tuple 'Z' h

        FieldContinuation _ ->
          Tuple '+' h

        Comment _ ->
            Tuple '-' h

        UnsupportedHeader ->
          Tuple 'u' h

    annotatedHeaders =
      map f $ reverse t.headers
  in
    fromFoldable annotatedHeaders


-- | Get the key signature (if any) from the tune.
getKeySig :: AbcTune -> Maybe ModifiedKeySignature
getKeySig tune =
  case (getHeader 'K' tune) of
    Just (Key key) ->
      Just key
    _ ->
      Nothing

-- | Get the meter
getMeter :: AbcTune -> Maybe MeterSignature
getMeter tune =
  case (getHeader 'M' tune) of
    Just (Meter maybeMeter) ->
      Just (fromMaybe (Tuple 4 4) $ maybeMeter)
    _ ->
      Nothing

-- | get the tempo
getTempoSig :: AbcTune -> Maybe TempoSignature
getTempoSig tune =
  case (getHeader 'Q' tune) of
    Just (Tempo tempo) ->
      Just tempo
    _ ->
      Nothing

-- | Get the first Title (if any) from the tune.
getTitle :: AbcTune -> Maybe String
getTitle tune =
  case (getHeader 'T' tune) of
    Just (Title title) ->
      Just title
    _ ->
      Nothing

-- | Get the unit note length
getUnitNoteLength :: AbcTune -> Maybe NoteDuration
getUnitNoteLength tune =
  case (getHeader 'L' tune) of
    Just (UnitNoteLength duration) ->
      Just duration
    _ ->
      Nothing

-- | Get the header from the header code
getHeader :: Char -> AbcTune -> Maybe Header
getHeader code t =
  lookup code (getHeaderMap t)

-- | The amount by which you increase or decrease the duration of a (possibly multiply) dotted note.
-- |    For example A > B increases the duration of A and proportionally reduces that of B.
-- |    A << B decreases the duration of A and increases that of B by an even greater amount.  This function
-- |    calculates the increase or decrease.  The new duration will be given by:
-- |
-- |    duration * (1 +/- dotfactor i)
-- |
-- |   i is the number of 'dot' indicators (< or >)
-- |
dotFactor :: Int -> Rational
dotFactor i =
  case i of
    1 ->
      1 % 2

    2 ->
      3 % 4

    3 ->
      7 % 8

    _ ->
      0 % 1

-- | check if a new stave's contents is effectively empty
-- | (the list of bars is introduced by the Score BodyPart)
isEmptyStave :: List Bar -> Boolean
isEmptyStave bars =
  all isEmptyBar bars
    where
      isEmptyBar :: Bar -> Boolean
      isEmptyBar bar =
        let
          f music' =
            case music' of
              Spacer _ ->
                true
              Ignore ->
                true
              Continuation _ ->
                true
              _ ->
                false
        in
          all f bar.music || null bar.music
