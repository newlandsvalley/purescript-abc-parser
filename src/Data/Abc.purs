-- | ABC data types
module Data.Abc
  ( AbcTune
  , TuneHeaders
  , TuneBody
  , BodyPart(..)
  , MusicLine
  , Header(..)
  , Music(..)
  , AbcRest
  , AbcNote
  , Grace
  , GraceableNote
  , AbcChord
  , RestOrNote
  , AbcTuplet
  , Bar
  , BarLine
  , Thickness(..)
  , Volta(..)
  , NoteDuration
  , KeySignature
  , ModifiedKeySignature
  , Pitch(..)
  , KeySet
  , TempoSignature
  , TimeSignature
  , TupletSignature(..)
  , AnnotationPlacement(..)
  , Mode(..)
  , Accidental(..)
  , PitchClass(..)
  , SymbolDefinition
  , Broken(..)
  , VoiceDescription(..)
  , AmorphousProperties
  , middlecOctave
  ) where

import Data.Either (Either)
import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering(..))
import Data.Rational (Rational)
import Data.String (toLower) as Str
import Prelude (class Show, class Eq, class Ord, (<>), compare, show)

-- | A Tune.
type AbcTune =
  { headers :: TuneHeaders
  , body :: TuneBody
  }

-- | A List of Tune Headers.
type TuneHeaders =
  List Header

-- | A Tune Body.
type TuneBody =
  List BodyPart

-- | A Tune Body part
data BodyPart
  = Score (List Bar)
  | BodyInfo Header

-- | A music phrase is contained within a Bar which is a set of music items
-- | introduced by a bar line
type Bar =
  { decorations :: List String
  , startLine :: BarLine -- we only consider the start line of each bar
  , music :: List Music
  }

-- | A line of musical within a bar.
type MusicLine =
  List Music

-- | A Rest.
type AbcRest =
  { duration :: NoteDuration }

-- | A Note.
type AbcNote =
  { pitchClass :: PitchClass
  , accidental :: Accidental
  , octave :: Int
  , duration :: NoteDuration
  , tied :: Boolean -- to the next note
  }

-- | a set of one or more grace notes
type Grace =
  { isAcciaccatura :: Boolean
  , notes :: NonEmptyList AbcNote
  }

-- | A (possibly) Graced and Decorated Note.
type GraceableNote =
  { maybeGrace :: Maybe Grace
  , leftSlurs :: Int
  , decorations :: List String
  , abcNote :: AbcNote
  , rightSlurs :: Int
  }

-- | A Chord.
type AbcChord =
  { leftSlurs :: Int
  , decorations :: List String
  , notes :: NonEmptyList AbcNote
  , duration :: NoteDuration
  , rightSlurs :: Int
  }

-- | A Chord Symbol definition.  Note that in ABC, this is merely a free-format string 
-- | with recommendations in the spec of how it might be properly formatted.
-- | We include an optional duration.  This is ignored by the parser but can 
-- | act as a placeholder for applications such as players to calculate a duration.
type SymbolDefinition =
  { name :: String
  , duration :: Maybe NoteDuration
  }

-- | A tuplet signature:
-- |    put p notes into the time of q the next r notes.
type TupletSignature =
  { p :: Int
  , q :: Int
  , r :: Int
  }

-- | A Tuplet
type AbcTuplet =
  { maybeGrace :: Maybe Grace
  , leftSlurs :: Int
  , signature :: TupletSignature
  , restsOrNotes :: NonEmptyList RestOrNote
  }

-- | An Annotation placement.
data AnnotationPlacement
  = AboveNextSymbol
  | BelowNextSymbol
  | LeftOfNextSymbol
  | RightOfNextSymbol
  | Discretional

instance showAnnotationPlacement :: Show AnnotationPlacement where
  show AboveNextSymbol = "^"
  show BelowNextSymbol = "_"
  show LeftOfNextSymbol = "<"
  show RightOfNextSymbol = ">"
  show Discretional = "@"

-- | either a Rest or a Note.
type RestOrNote
  = Either AbcRest GraceableNote

-- | The 'score' part of Music.
data Music
  = Note GraceableNote
  | BrokenRhythmPair RestOrNote Broken RestOrNote
  | Rest AbcRest
  | Tuplet AbcTuplet
  | DecoratedSpace (List String)
  | Annotation AnnotationPlacement String
  | ChordSymbol SymbolDefinition
  | Chord AbcChord
  | Inline Header
  | Spacer Int
  | Ignore
  | Continuation String

-- | A bar line Thickness.
data Thickness
  = Thin
  | ThinThin
  | ThinThick
  | ThickThin
  | Invisible -- | e.e. an implied bar line at the start of a stave

instance showThickness :: Show Thickness where
  show Thin = "|"
  show ThinThin = "||"
  show ThinThick = "|]"
  show ThickThin = "[|"
  show Invisible = ""

derive instance eqThickness :: Eq Thickness

-- | a Volta - a repeated section
data Volta
  = Volta Int -- |1  or |2 etc
  | VoltaRange Int Int -- |1-3 etc

derive instance genericVolta :: Generic Volta _
instance showVolta :: Show Volta where
  show (Volta v) = show v
  show (VoltaRange v1 v2) = (show v1) <> "-" <> (show v2)

derive instance eqVolta :: Eq Volta

{-| A Bar Line:
*  endRepeats - any repeat of the previous section indicated by colon(s)
*  thickness - the thickness of vertical lines in the bar
*  startRepeats - any repeat of the section to follow indicated by colon(s)
*  iteration - the section end may be an iteration defined by volta markings
-}
type BarLine =
  { endRepeats :: Int
  , thickness :: Thickness
  , startRepeats :: Int
  , iteration :: Maybe (NonEmptyList Volta)
  }

-- | A Mode.
data Mode
  = Major
  | Minor
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian

instance showMode :: Show Mode where
  show Major = "Major"
  show Minor = "Minor"
  show Ionian = "Ionian"
  show Dorian = "Dorian"
  show Phrygian = "Phrygian"
  show Lydian = "Lydian"
  show Mixolydian = "Mixolydian"
  show Aeolian = "Aeolian"
  show Locrian = "Locrian"

derive instance eqMode :: Eq Mode

-- | An Accidental.
data Accidental
  = Sharp
  | Flat
  | DoubleSharp
  | DoubleFlat
  | Natural
  | Implicit -- accidental determoined by context of the note

{- as shown in the body of the tune but not in headers -}
instance showAccidental :: Show Accidental where
  show Sharp = "^"
  show Flat = "_"
  show DoubleSharp = "^^"
  show DoubleFlat = "__"
  show Natural = "="
  show Implicit = ""

derive instance eqAccidental :: Eq Accidental
derive instance ordAccidental :: Ord Accidental

-- | A white note on the piano.
data PitchClass
  = A
  | B
  | C
  | D
  | E
  | F
  | G

instance showPitchClass :: Show PitchClass where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show F = "F"
  show G = "G"

derive instance eqPitchCLass :: Eq PitchClass
derive instance ordPitchCLass :: Ord PitchClass

instance enumPitchClass :: Enum PitchClass where
  succ C = Just D
  succ D = Just E
  succ E = Just F
  succ F = Just G
  succ G = Just A
  succ A = Just B
  succ B = Just C

  pred C = Just B
  pred D = Just C
  pred E = Just D
  pred F = Just E
  pred G = Just F
  pred A = Just G
  pred B = Just A

-- | A Key Signature.
type KeySignature =
  { pitchClass :: PitchClass
  , accidental :: Accidental -- Sharp, Flat or (explicitly) Natural
  , mode :: Mode
  }

-- | A Key Signature with modifications (possibly empty)
-- |    This is used for non-diatonicrepeatS modes where intervals may be greater than two semitones
-- |    (for example as found in Klezmer).
type ModifiedKeySignature =
  { keySignature :: KeySignature
  , modifications :: List Pitch
  , properties :: AmorphousProperties
  }

-- | A Key Accidental is represented by a Pitch (A modification to a standard key for one pitch in the scale).
-- |  (we're not allowed to derive instances on record types unless we use newtype)
data Pitch = Pitch
  { pitchClass :: PitchClass
  , accidental :: Accidental
  }

derive instance genericPitchClass :: Generic PitchClass _
derive instance genericAccidental :: Generic Accidental _

derive instance genericPitch :: Generic Pitch _

instance eqPitch :: Eq Pitch where
  eq = genericEq

instance ordPitch :: Ord Pitch where
  compare (Pitch r1) (Pitch r2) =
    let
      comp1 = compare r1.pitchClass r2.pitchClass
    in
      case comp1 of
        EQ ->
          compare r1.accidental r2.accidental
        _ ->
          comp1

instance showPitch :: Show Pitch where
  show (Pitch p) = show p.accidental <> Str.toLower (show p.pitchClass)

-- | A set of pitches within a key signature or scale
type KeySet =
  List Pitch

-- | A Time Signature - e.g. 3/4.
type TimeSignature = 
  { numerator :: Int 
  , denominator :: Int 
  }

{-| A Tempo Signature - for example:
*  1/4=120
*  1/4 3/8 1/4 3/8=40   (up to 4 note lengths allowed)
*  "Allegro" 1/4=120
*  3/8=50 "Slowly".
-}
type TempoSignature =
  { noteLengths :: NonEmptyList Rational
  , bpm :: Int
  , marking :: Maybe String
  }

-- | A Note Duration - e.g. 1/4.
-- newtype NoteDuration = NoteDuration Rational
type NoteDuration = Rational

-- | A broken rhythm operator - one or more < or >.-}
data Broken
  = LeftArrow Int
  | RightArrow Int

-- | voice description
type VoiceDescription =
  { id :: String
  , properties :: AmorphousProperties
  }

-- | An ABC Tune Header.
data Header
  = Area String
  | Book String
  | Composer String
  | Discography String
  | FileUrl String
  | Group String
  | History String
  | Instruction String
  -- Directive
  | Key ModifiedKeySignature
  -- a standard key signature possibly modified with accidentals
  | UnitNoteLength NoteDuration
  | Meter (Maybe TimeSignature)
  | Macro String
  | Notes String
  | Origin String
  | Parts String
  | Tempo TempoSignature
  | Rhythm String
  | Remark String
  | Source String
  | SymbolLine String
  | Title String
  | UserDefined String
  | Voice VoiceDescription
  -- voice properties
  | WordsAfter String
  -- words after notes
  | WordsAligned String
  -- words aligned with notes
  | ReferenceNumber (Maybe Int)
  -- spec says number is optionally allowed to be blank but not recommended
  | Transcription String
  | FieldContinuation String
  | Comment String
  | UnsupportedHeader

-- | we stringly-type any header properties that we don't particularly care about
type AmorphousProperties = Map String String

-- | The octave number of middle C in MIDI parlance.
middlecOctave :: Int
middlecOctave =
  5
