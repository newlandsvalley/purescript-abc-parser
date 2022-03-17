-- | Conversion of an ABC pitch to a MIDI pitch
module Data.Abc.Midi.Pitch 
 ( MidiPitch
 , toMidiPitch
 , midiPitchOffset
 ) 

where 

import Data.Abc (AbcNote, Accidental(..), ModifiedKeySignature, Pitch(..))
import Data.Abc.Accidentals as Accidentals
import Data.Abc.KeySignature (modifiedKeySet, pitchNumber, notesInChromaticScale)
import Data.Foldable (oneOf)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Prelude ((+), (*), ($))

-- | The pitch of a note expressed as a MIDI interval.
type MidiPitch =
  Int

-- | Convert an ABC note pitch to a MIDI pitch.
-- |
-- | AbcNote - the note in question
-- | ModifiedKeySignature - the key signature (possibly modified by extra accidentals)
-- | Accidentals - any notes in this bar which have previously been set explicitly to an accidental which are thus inherited by this note
-- | MidiPitch - the resulting pitch of the MIDI note
toMidiPitch :: ModifiedKeySignature -> Accidentals.Accidentals -> AbcNote -> MidiPitch
toMidiPitch mks barAccidentals n =
  (n.octave * notesInChromaticScale) + midiPitchOffset mks barAccidentals n

-- | convert an AbcNote (pich class and accidental) to a pitch offset in a chromatic scale
midiPitchOffset :: ModifiedKeySignature -> Accidentals.Accidentals -> AbcNote -> Int
midiPitchOffset mks barAccidentals n =
  let
    inBarAccidental =
      Accidentals.lookup n.pitchClass barAccidentals

    inKeyAccidental =
      -- accidentalImplicitInKey n.pitchClass mks
      Accidentals.implicitInKeySet n.pitchClass (modifiedKeySet mks)

    -- look first for an explicit note accidental, then for an explicit for the same note that occurred earlier in the bar and
    -- finally look for an implicit accidental attached to this key signature
    accidental =
      case n.accidental of
        Implicit ->
          fromMaybe Natural $ oneOf (inBarAccidental : inKeyAccidental : Nil)
        _ -> -- explict
          n.accidental

    pattern =
      Pitch { pitchClass: n.pitchClass, accidental: accidental }
  in
    pitchNumber pattern
