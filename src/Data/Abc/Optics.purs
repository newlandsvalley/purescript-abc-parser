-- | Optics for accessing the tune headers
-- |
-- | ABC allows for multiple headers of the same type to exist: 
-- | https://abcnotation.com/wiki/abc:standard:v2.1#file_header
-- | It's not clear how you should discriminate between them if, for example,
-- | you want to display the main tune header on a score.  I use the convention
-- | that the first header to be defined is the prominent one.
-- |
-- | It's also probably not necessary that optics are needed to retrieve components of
-- | the tune body because it's almost always necessary to process this serially.
-- |
-- | Usage requires profunctor-optics.
-- |
-- | To retrieve the first title:
-- |
-- | firstOf (_headers <<< traversed <<< _Title) abcTune
-- |
-- | To retrieve all the titles into a list:
-- |
-- | toListOf (_headers <<< traversed <<< _Title) abcTune
-- |
-- | Or to get the tune mode:
-- | 
-- | firstOf (_headers <<< traversed <<< _ModifiedKeySignature <<< _keySignature <<< _mode) abcTune
-- |
-- | Or to reset the title of a tune:
-- |
-- | set (_headers <<< traversed <<< _Title) "new title" abcTune
-- |

module Data.Abc.Optics where

import Data.Abc
import Data.Maybe (Maybe(..))
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

-- | the tune headers
_headers :: forall a r. Lens' { headers :: a | r } a
_headers = prop (Proxy :: Proxy "headers")

-- | the tune body
_body :: forall a r. Lens' { body :: a | r } a
_body = prop (Proxy :: Proxy "body")

-- | specific headers

_Area :: Prism' Header String
_Area = prism' Area case _ of
  Area a -> Just a
  _ -> Nothing

_Book :: Prism' Header String
_Book = prism' Book case _ of
  Book a -> Just a
  _ -> Nothing

_Composer :: Prism' Header String
_Composer = prism' Composer case _ of
  Composer a -> Just a
  _ -> Nothing

_Discography :: Prism' Header String
_Discography = prism' Discography case _ of
  Discography a -> Just a
  _ -> Nothing

_FileUrl :: Prism' Header String
_FileUrl = prism' FileUrl case _ of
  FileUrl a -> Just a
  _ -> Nothing

_Group :: Prism' Header String
_Group = prism' Group case _ of
  Group a -> Just a
  _ -> Nothing

_History :: Prism' Header String
_History = prism' History case _ of
  History a -> Just a
  _ -> Nothing

_Instruction :: Prism' Header String
_Instruction = prism' Instruction case _ of
  Instruction a -> Just a
  _ -> Nothing

_ModifiedKeySignature :: Prism' Header ModifiedKeySignature
_ModifiedKeySignature = prism' Key case _ of
  Key mks -> Just mks
  _ -> Nothing

_UnitNoteLength :: Prism' Header NoteDuration
_UnitNoteLength = prism' UnitNoteLength case _ of
  UnitNoteLength a -> Just a
  _ -> Nothing

_Meter :: Prism' Header (Maybe TimeSignature)
_Meter = prism' Meter case _ of
  Meter maybeTs -> Just maybeTs
  _ -> Nothing

_Macro :: Prism' Header String
_Macro = prism' Macro case _ of
  Macro a -> Just a
  _ -> Nothing

_Notes :: Prism' Header String
_Notes = prism' Notes case _ of
  Notes a -> Just a
  _ -> Nothing

_Origin :: Prism' Header String
_Origin = prism' Origin case _ of
  Origin a -> Just a
  _ -> Nothing

_Parts :: Prism' Header String
_Parts = prism' Parts case _ of
  Parts a -> Just a
  _ -> Nothing

_Tempo :: Prism' Header TempoSignature
_Tempo = prism' Tempo case _ of
  Tempo a -> Just a
  _ -> Nothing

_Rhythm :: Prism' Header String
_Rhythm = prism' Rhythm case _ of
  Rhythm a -> Just a
  _ -> Nothing

_Remark :: Prism' Header String
_Remark = prism' Remark case _ of
  Remark a -> Just a
  _ -> Nothing

_Source :: Prism' Header String
_Source = prism' Source case _ of
  Source a -> Just a
  _ -> Nothing

_SymbolLine :: Prism' Header String
_SymbolLine = prism' SymbolLine case _ of
  SymbolLine a -> Just a
  _ -> Nothing

_Title :: Prism' Header String
_Title = prism' Title case _ of
  Title a -> Just a
  _ -> Nothing

_Voice :: Prism' Header VoiceDescription
_Voice = prism' Voice case _ of
  Voice a -> Just a
  _ -> Nothing

_WordsAfter :: Prism' Header String
_WordsAfter = prism' WordsAfter case _ of
  WordsAfter a -> Just a
  _ -> Nothing

_WordsAligned :: Prism' Header String
_WordsAligned = prism' WordsAligned case _ of
  WordsAligned a -> Just a
  _ -> Nothing

_ReferenceNumber :: Prism' Header (Maybe Int)
_ReferenceNumber = prism' ReferenceNumber case _ of
  ReferenceNumber maybeRef -> Just maybeRef
  _ -> Nothing

_Transcription :: Prism' Header String
_Transcription = prism' Transcription case _ of
  Transcription a -> Just a
  _ -> Nothing

_FieldContinuation :: Prism' Header String
_FieldContinuation = prism' FieldContinuation case _ of
  FieldContinuation a -> Just a
  _ -> Nothing

_Comment :: Prism' Header String
_Comment = prism' Comment case _ of
  Comment a -> Just a
  _ -> Nothing

-- | modified Key signature fields

-- | the underlying key signature
_keySignature :: forall a r. Lens' { keySignature :: a | r } a
_keySignature = prop (Proxy :: Proxy "keySignature")

-- | modifications to the key signature
_keySignatureModifications :: forall a r. Lens' { modifications :: a | r } a
_keySignatureModifications = prop (Proxy :: Proxy "modifications")

-- | the pitch class e.g. A
_pitchClass :: forall a r. Lens' { pitchClass :: a | r } a
_pitchClass = prop (Proxy :: Proxy "pitchClass")

-- the accidental e.g. Sharp
_accidental :: forall a r. Lens' { accidental :: a | r } a
_accidental = prop (Proxy :: Proxy "accidental")

-- the mode of the key signature e.g. Major
_mode :: forall a r. Lens' { mode :: a | r } a
_mode = prop (Proxy :: Proxy "mode")

-- | Tempo signature fields

-- bpm (beats per minute) 
_bpm :: forall a r. Lens' { bpm :: a | r } a
_bpm = prop (Proxy :: Proxy "bpm")

-- the note lengths
_noteLengths :: forall a r. Lens' { noteLengths :: a | r } a
_noteLengths = prop (Proxy :: Proxy "noteLengths")

_marking :: forall a r. Lens' { marking :: a | r } a
_marking = prop (Proxy :: Proxy "marking")

-- | Voice fields
_id :: forall a r. Lens' { id :: a | r } a
_id = prop (Proxy :: Proxy "id")

-- | key signature or voice description properties
_properties :: forall a r. Lens' { properties :: a | r } a
_properties = prop (Proxy :: Proxy "properties")

