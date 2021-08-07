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

{-}
-- | A representation of the ABC headers as a Map, taking each definition
-- | of any header in presentation order where multiple definitions are 
-- | present in the ABC.
type HeaderMap =
    Map Char (List Header)
-}

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
-- | @deprecated in favour of the optics
getKeySig :: AbcTune -> Maybe ModifiedKeySignature
getKeySig tune =
  firstOf (_headers <<< traversed <<< _ModifiedKeySignature) tune

{-}
  case (getHeader 'K' tune) of
    Just (Key key _) ->
      Just key
    _ ->
      Nothing
-}

{-}
getDefaultedKeyProps :: AbcTune -> AmorphousProperties
getDefaultedKeyProps tune =
  fromMaybe (empty :: AmorphousProperties) $ (_headers <<< traversed <<< (_KeySignatureProperties mks)) tune
-}

-- | Get the key signature properties (if any) from the tune.
getKeyProps :: AbcTune -> AmorphousProperties
getKeyProps tune =
  case (firstOf (_headers <<< traversed <<< _ModifiedKeySignature <<< _properties) tune) of
    Just props -> props 
    _ -> (empty :: AmorphousProperties)

{-
  case (getHeader 'K' tune) of
    Just (Key key props) ->
      props
    _ ->
      (empty :: AmorphousProperties)
-}

-- | Get the meter defaulting to 4/4
getDefaultedMeter :: AbcTune -> MeterSignature
getDefaultedMeter tune =
  fromMaybe (Tuple 4 4) $ getMeter tune

-- | @deprecated in favour of getDefaultedMeter
getMeter :: AbcTune -> Maybe MeterSignature
getMeter tune =
  join $ (firstOf (_headers <<< traversed <<< _Meter) tune) 
    
  
  {-}
  case (getHeader 'M' tune) of
    Just (Meter maybeMeter) ->
      Just (fromMaybe (Tuple 4 4) $ maybeMeter)
    _ ->
      Nothing
-}


-- | get the tempo
-- | @deprecated in favour of the optics
getTempoSig :: AbcTune -> Maybe TempoSignature
getTempoSig tune =
  firstOf (_headers <<< traversed <<< _Tempo) tune
{-}
  case (getHeader 'Q' tune) of
    Just (Tempo tempo) ->
      Just tempo
    _ ->
      Nothing
-}

-- | Get the first Title (if any) from the tune.
-- | @deprecated in favour of the optics
getTitle :: AbcTune -> Maybe String
getTitle tune =
  firstOf (_headers <<< traversed <<< _Title) tune

{-}
  case (getHeader 'T' tune) of
    Just (Title title) ->
      Just title
    _ ->
      Nothing
-}

-- | Get the unit note length
-- | @deprecated in favour of the optics
getUnitNoteLength :: AbcTune -> Maybe NoteDuration
getUnitNoteLength tune =
  firstOf (_headers <<< traversed <<< _UnitNoteLength) tune

  {-}
  case (getHeader 'L' tune) of
    Just (UnitNoteLength duration) ->
      Just duration
    _ ->
      Nothing
  -}

{-}
-- | Get the first header (in presentation order) from the header code
getHeader :: Char -> AbcTune -> Maybe Header
getHeader code t =
  case (lookup code (getHeaderMap t)) of 
    Nothing -> Nothing 
    Just headers -> head headers
    
-- | Get all matching headers (in presentation order) from the header code
getHeaders :: Char -> AbcTune -> List Header
getHeaders code t =
  case (lookup code (getHeaderMap t)) of 
    Nothing -> Nil
    Just headers -> headers
-}

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

-- IMPLEMENTATION
  

{-
-- | A map (Header code => List Header) for each instance of 
-- | each Header code  (in order of presentation)
getHeaderMap :: AbcTune -> HeaderMap
getHeaderMap t =
  let
    f :: Header -> Tuple Char (List Header)
    f h =
      case h of
        Area _ ->
          Tuple 'A' (singleton h)

        Book _ ->
          Tuple 'B' (singleton h)

        Composer _ ->
          Tuple 'C' (singleton h)

        Discography _ ->
          Tuple 'D' (singleton h)

        FileUrl _ ->
          Tuple 'F' (singleton h)

        Group _ ->
          Tuple 'G' (singleton h)

        History _ ->
          Tuple 'H' (singleton h)

        Instruction _ ->
          Tuple 'I' (singleton h)

        Key _ ->
          Tuple 'K' (singleton h)

        UnitNoteLength _ ->
          Tuple 'L' (singleton h)

        Meter _ ->
          Tuple 'M' (singleton h)

        Macro _ ->
          Tuple 'm' (singleton h)

        Notes _ ->
          Tuple 'N' (singleton h)

        Origin _ ->
          Tuple 'O' (singleton h)

        Parts _ ->
          Tuple 'P' (singleton h)

        Tempo _ ->
          Tuple 'Q' (singleton h)

        Rhythm _ ->
          Tuple 'R' (singleton h)

        Remark _ ->
          Tuple 'r' (singleton h)

        Source _ ->
          Tuple  'S' (singleton h)

        SymbolLine _ ->
          Tuple 's' (singleton h)

        Title _ ->
          Tuple 'T' (singleton h)

        UserDefined _ ->
          Tuple 'U' (singleton h)

        Voice _ ->
          Tuple 'V' (singleton h)

        WordsAfter _ ->
          Tuple 'W' (singleton h)

        WordsAligned _ ->
          Tuple  'w' (singleton h)

        ReferenceNumber _ ->
          Tuple 'X' (singleton h)

        Transcription _ ->
          Tuple 'Z' (singleton h)

        FieldContinuation _ ->
          Tuple '+' (singleton h)

        Comment _ ->
          Tuple '-' (singleton h)

        UnsupportedHeader ->
          Tuple 'u' (singleton h)

    annotatedHeaders =
      map f $ reverse t.headers
  in
    fromFoldableWith (<>) annotatedHeaders   
-}