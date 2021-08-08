-- | A Ragbag of convenience functions for getting Metadata from ABC
module Data.Abc.Metadata
        ( getKeySet
        , getKeySig
        , getKeyProps
        , getMeter
        , getDefaultedMeter
        , getTempoSig
        , getTitle
        , getUnitNoteLength
        , dotFactor
        , normaliseChord
        , isEmptyStave
        , thumbnail
        , removeRepeatMarkers
        ) where

import Data.Abc

import Data.Abc.KeySignature (modifiedKeySet)
import Data.Abc.Optics (_headers, _properties, _Meter, _ModifiedKeySignature, _Tempo, _Title, _UnitNoteLength)
import Data.Foldable (all)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
import Data.List (List(..), head, null, singleton, snoc, take)
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Rational (Rational, (%), toNumber)
import Data.Tuple (Tuple(..))
import Prelude (join, map, ($), (||), (==), (*), (<<<))

-- EXPORTED FUNCTIONS

-- | Get the set of key accidentals from the (possibly modified) key (if there is one in the tune).
getKeySet :: AbcTune -> KeySet
getKeySet t =
  case (getKeySig t) of
    Just ksig ->
      modifiedKeySet ksig
    Nothing ->
      Nil

-- | Get the key signature (if any) from the tune.
-- | For more flexibility, you should use the _ModifiedKeySignature optic.
getKeySig :: AbcTune -> Maybe ModifiedKeySignature
getKeySig tune =
  firstOf (_headers <<< traversed <<< _ModifiedKeySignature) tune

-- | Get the key signature properties (if any) from the tune.
getKeyProps :: AbcTune -> AmorphousProperties
getKeyProps tune =
  case (firstOf (_headers <<< traversed <<< _ModifiedKeySignature <<< _properties) tune) of
    Just props -> props 
    _ -> (empty :: AmorphousProperties)


-- | Get the meter defaulting to 4/4
getDefaultedMeter :: AbcTune -> MeterSignature
getDefaultedMeter tune =
  fromMaybe (Tuple 4 4) $ getMeter tune

-- | Get the tune Meter where present
-- | For more flexibility, you should use the _Meter optic.
getMeter :: AbcTune -> Maybe MeterSignature
getMeter tune =
  join $ (firstOf (_headers <<< traversed <<< _Meter) tune) 
  

-- | Get the tempo where present
-- | For more flexibility, you should use the _Tempo optic.
getTempoSig :: AbcTune -> Maybe TempoSignature
getTempoSig tune =
  firstOf (_headers <<< traversed <<< _Tempo) tune

-- | Get the first Title (if any) from the tune.
-- | For more flexibility, you should use the _Title optic.
getTitle :: AbcTune -> Maybe String
getTitle tune =
  firstOf (_headers <<< traversed <<< _Title) tune

-- | Get the unit note length
-- | For more flexibility, you should use the _UnitNoteLength optic.
getUnitNoteLength :: AbcTune -> Maybe NoteDuration
getUnitNoteLength tune =
  firstOf (_headers <<< traversed <<< _UnitNoteLength) tune

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

-- | Normalise an ABC chord by placing the correct duration against each note
-- | and setting the overall Chord length to Unit
normaliseChord :: AbcChord -> AbcChord
normaliseChord abcChord =
  case (toNumber abcChord.duration) of
    1.0 -> abcChord
    _ ->
      let
        notes = map (\n -> n { duration = n.duration * abcChord.duration} ) abcChord.notes
        decorations = abcChord.decorations
        leftSlurs = abcChord.leftSlurs
        rightSlurs = abcChord.rightSlurs
      in
        { leftSlurs, decorations, notes, duration : (1 % 1), rightSlurs }

-- filter the bars we need for the thumbnail and terminate properly with
-- an empty bar.
filterBars :: List Bar -> List Bar
filterBars bars =
  let
    -- identify whether we have a lead-in bar
    count =
      case head bars of
        Nothing ->
          0
        Just bar ->
          if (bar.startLine.thickness == Invisible)
            then 3
            else 2

    emptyBarLine :: BarLine
    emptyBarLine =    
      { endRepeats : 0
      , thickness : Thin
      , startRepeats : 0
      , iteration : Nothing
      }

    emptyBar :: Bar
    emptyBar =
      { decorations : Nil
      , startLine : emptyBarLine
      , music : Nil
      }
  in
    snoc (take count bars) emptyBar

-- | reduce an ABC tune to a 'thumbnail' of the first two full bars
thumbnail :: AbcTune -> AbcTune
thumbnail t =
  let
    f :: BodyPart -> List Bar
    f = case _ of
      Score bars -> bars
      _ -> Nil
    firstLine :: List Bar
    firstLine = maybe Nil f $ head t.body
    newBody = singleton (Score $ filterBars firstLine)
  in
    t { body = newBody }

-- | remove repeat markers (used for thumbnails where we need to ignore them)
removeRepeatMarkers :: AbcTune -> AbcTune
removeRepeatMarkers abcTune =

  { headers : abcTune.headers
  , body : replaceBody abcTune.body
  }

  where

    removeRepeat :: Bar -> Bar
    removeRepeat bar =
      let
        newStartLine = bar.startLine { startRepeats = 0, endRepeats = 0 }
      in
        bar { startLine = newStartLine }

    replaceBars :: List Bar -> List Bar
    replaceBars = map removeRepeat

    replaceBodyPart :: BodyPart -> BodyPart
    replaceBodyPart bp =
      case bp of
        Score bars ->
          Score $ replaceBars bars
        _ ->
          bp

    replaceBody  :: List BodyPart -> List BodyPart
    replaceBody = map replaceBodyPart
