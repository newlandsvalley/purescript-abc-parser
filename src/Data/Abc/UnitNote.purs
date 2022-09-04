-- | The length of a unit note - see 3.1.7 L: - unit note length
module Data.Abc.UnitNote
  ( defaultUnitNoteLength
  , getUnitNoteLength
  ) where

import Prelude ((/), (<), (<<<))
import Data.Abc (AbcTune, TimeSignature, NoteDuration)
import Data.Abc.Optics (_headers, _UnitNoteLength)
import Data.Int (toNumber)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
import Data.Maybe (Maybe)
import Data.Rational ((%))

-- | Get the unit note length
-- | For more flexibility, you should use the _UnitNoteLength optic.
getUnitNoteLength :: AbcTune -> Maybe NoteDuration
getUnitNoteLength tune =
  firstOf (_headers <<< traversed <<< _UnitNoteLength) tune

-- calculate the default unit note length from the Meter 
-- signature (which defaults to 4/4)
defaultUnitNoteLength :: TimeSignature -> NoteDuration
defaultUnitNoteLength sig =
  let
    computedMeter :: Number
    computedMeter =
      case sig of
        { numerator, denominator} -> (toNumber numerator) / (toNumber denominator)
  in
    if (computedMeter < 0.75) then
      (1 % 16)
    else
      (1 % 8)