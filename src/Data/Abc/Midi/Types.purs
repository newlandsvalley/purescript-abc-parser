module Data.Abc.Midi.Types 
  ( MidiBar) where

import Data.Maybe (Maybe)
import Data.List (List)
import Data.Abc (Volta)
import Data.Midi as Midi

-- | a bar of MIDI music
type MidiBar =
  { number :: Int                         -- sequential from zero
  , endRepeats :: Int                     -- an end repeat (n >= 0)
  , startRepeats :: Int                   -- a start repeat (n >= 0)
  , iteration :: Maybe Volta              -- an iteration volta marker  (|1  or |2 etc)
  , midiMessages :: List Midi.Message     -- the notes in the bar or any tempo changes
  }
