module Data.Abc.Midi.Types 
  ( MidiBar
  , Section(..)
  , Sections
  , RepeatState) where


import Prelude (class Eq, class Show)
import Data.Generic.Rep
import Data.Maybe (Maybe)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
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

-- | a section of the tune (possibly repeated)
newtype Section = Section
    { start :: Maybe Int
    , firstEnding :: Maybe Int
    , secondEnding :: Maybe Int
    , end :: Maybe Int
    , isRepeated :: Boolean
    }
 
derive instance newtypeSection :: Newtype Section _
derive instance genericSection :: Generic Section _
instance eqSection :: Eq Section where  eq = genericEq
instance showSection :: Show Section where show = genericShow   

-- | a set of sections
type Sections = List Section

-- | the current repeat state
type RepeatState =
    { current :: Section
    , sections :: Sections
    }  