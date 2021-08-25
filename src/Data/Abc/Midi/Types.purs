module Data.Abc.Midi.Types
  ( MidiBar
  , MidiBars
  ) where

import Data.Maybe (Maybe)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Abc (Volta)
import Data.Midi as Midi

-- | a bar of MIDI music
type MidiBar =
  { number :: Int -- sequential from zero
  , endRepeats :: Int -- an end repeat (n >= 0)
  , startRepeats :: Int -- a start repeat (n >= 0)
  , iteration :: Maybe (NonEmptyList Volta) -- an iteration volta marker  (|1  or |2 or |1-3 etc)
  , midiMessages :: List Midi.Message -- the notes in the bar or any tempo changes
  }

type MidiBars = List MidiBar
