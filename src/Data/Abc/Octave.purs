-- | Conversion to a new octave.
module Data.Abc.Octave
        ( move
        , up
        , down
        ) where

import Prelude ((+), map, negate)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Either (Either(..))
import Data.Abc

-- import Test.Unit.Assert as Assert
-- Exposed API

-- | if true, then move the tune up an octave, else down.
move :: Boolean -> AbcTune -> AbcTune
move isUp =
  if isUp then
    up
  else
    down

-- | Move the tune up an octave.
up :: AbcTune -> AbcTune
up t =
  moveTune 1 t


-- | Move the tune down octave.
down :: AbcTune -> AbcTune
down t =
  moveTune (-1) t

-- Implementation
moveTune :: Int -> AbcTune -> AbcTune
moveTune i t =
  { headers: t.headers, body: (moveTuneBody i t.body) }


moveTuneBody :: Int -> TuneBody -> TuneBody
moveTuneBody i =
    map (moveBodyPart i)

moveBodyPart :: Int -> BodyPart -> BodyPart
moveBodyPart i bp =
    case bp of
        Score ms ->
            Score (moveBarList i ms)

        _ ->
            bp

moveOctave :: Int -> Music -> Music
moveOctave i m =
    case m of
        Note n ->
            Note (moveGraceableNoteBy i n)

        BrokenRhythmPair n1 b n2 ->
            BrokenRhythmPair (moveGraceableNoteBy i n1) b (moveGraceableNoteBy i n2)

        Tuplet ts ns ->
            Tuplet ts (moveRestOrNoteList i ns)

        Chord c ->
            Chord (moveChord i c)

        _ ->
            m

moveGraceableNoteBy :: Int -> GraceableNote -> GraceableNote
moveGraceableNoteBy i gn =
  let
    abcNote = moveNoteBy i gn.abcNote
    maybeGrace = map (moveGraceBy i) gn.maybeGrace
    decorations = gn.decorations
  in
    { maybeGrace, decorations, abcNote }

moveNoteBy :: Int -> AbcNote -> AbcNote
moveNoteBy i note =
  note { octave = note.octave + i }

moveGraceBy :: Int -> Grace -> Grace
moveGraceBy i g =
  g { notes = moveNoteList i g.notes  }

moveBarList :: Int -> List Bar -> List Bar
moveBarList i =
  map (moveBar i)

moveBar :: Int -> Bar -> Bar
moveBar i bar =
  let
    newMusic =
      map (moveOctave i) bar.music
  in
    bar { music = newMusic }

moveNoteList :: Int -> NonEmptyList AbcNote -> NonEmptyList AbcNote
moveNoteList i =
    map (moveNoteBy i)

-- | tuples may now contain either rests or notes
moveRestOrNoteList :: Int -> NonEmptyList RestOrNote -> NonEmptyList RestOrNote
moveRestOrNoteList i =
  let
    f :: RestOrNote -> RestOrNote
    f rn =
      case rn of
        Left r -> Left r
        Right n -> Right (moveGraceableNoteBy i n)
  in
    map f

moveChord :: Int -> AbcChord -> AbcChord
moveChord i c =
    c { notes = moveNoteList i c.notes }
