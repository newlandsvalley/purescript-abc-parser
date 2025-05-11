-- | A Ragbag of convenience functions, many of which get Metadata from ABC
module Data.Abc.Utils
  ( getTitle
  , dotFactor
  , chordDuration
  , tupletDuration
  , isEmptyStave
  , thumbnail
  , removeRepeatMarkers
  ) where

import Data.Abc

import Data.Abc.Optics (_headers, _Title)
import Data.Either (Either(..))
import Data.Foldable (all, foldr)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
import Data.List (List(..), head, null, singleton, snoc, take)
import Data.List.NonEmpty (head) as NEL
import Data.Maybe (Maybe(..), maybe)
import Data.Rational (Rational, (%), fromInt)
import Data.String.Common (trim)
import Prelude (map, ($), (||), (==), (*), (+), (<<<))

-- | Get the first Title (if any) from the tune.
-- | For more flexibility, you should use the _Title optic.
getTitle :: AbcTune -> Maybe String
getTitle tune =
  map trim $ firstOf (_headers <<< traversed <<< _Title) tune

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



-- | Get the duration of a chord. We consider notes in a chord to have the same 
-- | duration (as the first such note) and must also cater for the overall chord duration. 
chordDuration :: AbcChord -> NoteDuration
chordDuration chord =
  (NEL.head chord.notes).duration * chord.duration

restOrNoteDuration :: RestOrNote -> NoteDuration
restOrNoteDuration =
  case _ of
    Left r ->
      r.duration
    Right gn ->
      gn.abcNote.duration

-- | Get the overall duration of a tuplet
tupletDuration :: AbcTuplet -> NoteDuration
tupletDuration t =
  modifier * foldr adder (fromInt 0) t.restsOrNotes

  where
  adder :: RestOrNote -> NoteDuration -> NoteDuration
  adder rorn acc = restOrNoteDuration rorn + acc

  modifier = t.signature.q % t.signature.p

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
          if (bar.startLine.thickness == Invisible) then 3
          else 2

    emptyBarLine :: BarLine
    emptyBarLine =
      { endRepeats: 0
      , thickness: Thin
      , startRepeats: 0
      , iteration: Nothing
      }

    emptyBar :: Bar
    emptyBar =
      { decorations: Nil
      , startLine: emptyBarLine
      , music: Nil
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

  { headers: abcTune.headers
  , body: replaceBody abcTune.body
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

  replaceBody :: List BodyPart -> List BodyPart
  replaceBody = map replaceBodyPart

          
