module Abc.ParseTree ( AbcTune
, TuneHeaders
, TuneBody
, BodyPart(..)
, MusicLine
, Header(..)
, Music(..)
, AbcNote
, AbcChord
, Bar
, Thickness(..)
, Repeat(..)
, NoteDuration
, KeySignature
, ModifiedKeySignature
, KeyAccidental
, KeySet
, MeterSignature(..)
, TempoSignature
, TupletSignature(..)
, AnnotationPlacement(..)
, Mode(..)
, Accidental(..)
, PitchClass(..)
, Broken(..)
, middlecOctave
) where


import Prelude (class Show)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Rational (Rational)
import Data.Tuple (Tuple)

{-| A Tune.-}
type AbcTune =
  { headers :: TuneHeaders
  , body :: TuneBody
  }


{-| A List of Tune Headers.-}
type TuneHeaders =
    List Header


{-| A Tune Body.-}
type TuneBody =
    List BodyPart


{-| A Tune Body part-}
data BodyPart
    = Score MusicLine
    | BodyInfo Header


{-| A line of musical score up to eol.-}
type MusicLine =
    List Music


{-| A Note.-}
type AbcNote =
    { pitchClass :: PitchClass
    , accidental :: Maybe Accidental
    , octave :: Int
    , duration :: NoteDuration
    , tied ::  Boolean  -- to the next note
    }


{-| A Chord.-}
type AbcChord =
    { notes :: List AbcNote
    , duration :: NoteDuration
    }


{-| An Annotation placement.-}
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


{-| The 'score' part of Music.-}
data Music
    = Barline Bar
    | Note AbcNote
    | BrokenRhythmPair AbcNote Broken AbcNote
    | Rest NoteDuration
    | Tuplet TupletSignature (List AbcNote)
    | Decoration String
    | Slur Char
    | GraceNote Boolean (List AbcNote)
      -- Music restricted to note sequences or chords
    | Annotation AnnotationPlacement String
    | ChordSymbol String
    | Chord AbcChord
    | Inline Header
    | Spacer Int
    | Ignore
    | Continuation


{-| A bar line Thickness.-}
data Thickness
    = Thin
    | ThinThin
    | ThinThick
    | ThickThin

instance showThickness :: Show Thickness where
  show Thin = "|"
  show ThinThin = "||"
  show ThinThick = "|]"
  show ThickThin = "[|"

{-| A Repeat in a Bar line.-}
data Repeat
    = Begin
    | End
    | BeginAndEnd

{-
instance showRepeat :: Show Repeat where
  show Begin = "|:"
  show End = "|:"
  show BeginAndEnd = ":|:"
  -}

{-| A Bar line:

*  thickness - the thickness of vertical lines in the bar
*  repeat - the type (if any) of a repeat marker for the section
*  iteration - the section end may be iteration 1 or 2.
-}
type Bar =
    { thickness :: Thickness
    , repeat :: Maybe Repeat
    , iteration :: Maybe Int
    }


{-| A Mode.-}
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

{-| An Accidental.-}
data Accidental
    = Sharp
    | Flat
    | DoubleSharp
    | DoubleFlat
    | Natural

{- as shown in the body of the tune but not in headers -}
instance showAccidental :: Show Accidental where
    show Sharp = "^"
    show Flat =  "_"
    show DoubleSharp = "^^"
    show DoubleFlat = "__"
    show Natural = "="


{-| A white note on the piano.-}
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



{-| A Key Signature.-}
type KeySignature =
    { pitchClass :: PitchClass
    , accidental :: Maybe Accidental
    , mode :: Mode
    }

{-| A Key Signature with modifications (possibly empty)
    This is used for non-diatonic modes where intervals may be greater than two semitones
    (for example as found in Klezmer).
-}
type ModifiedKeySignature =
  { keySignature :: KeySignature
  , modifications ::  List KeyAccidental
  }


{-| A Key Accidental (A modification to a standard key for one pitch in the scale).
-}
type KeyAccidental =
    { pitchClass :: PitchClass
    , accidental :: Accidental
    }


{-| A set of accidentals within a key signature.
-}
type KeySet =
    List KeyAccidental


{-| A Meter Signature - e.g. 3/4.
-}
type MeterSignature = Tuple Int Int


{-| A Tempo Signature - for example:

*  1/4=120
*  1/4 3/8 1/4 3/8=40   (up to 4 note lengths allowed)
*  "Allegro" 1/4=120
*  3/8=50 "Slowly".
-}
type TempoSignature =
    { noteLengths :: List Rational
    , bpm :: Int
    , marking :: Maybe String
    }


{-| A Note Duration - e.g. 1/4.
-}
-- newtype NoteDuration = NoteDuration Rational
type NoteDuration = Rational


{-| A tuplet signature:
    put p notes into the time of q the next r notes.
-}
type TupletSignature =
    { p :: Int
    , q :: Int
    , r :: Int
    }


{-| A broken rhythm operator - one or more < or >.-}
data Broken
    = LeftArrow Int
    | RightArrow Int


{-| An ABC Tune Header.-}
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
    | Meter (Maybe MeterSignature)
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
    | Voice String
      -- voice properties
    | WordsAfter String
      -- words after notes
    | WordsAligned String
      -- words aligned with notes
    | ReferenceNumber Int
    | Transcription String
    | FieldContinuation String
    | Comment String
    | UnsupportedHeader


{-| The octave number of middle C in MIDI parlance.
-}
middlecOctave :: Int
middlecOctave =
    5
