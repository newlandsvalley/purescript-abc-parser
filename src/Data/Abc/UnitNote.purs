-- | The length of a unit note - see 3.1.7 L: - unit note length
module Data.Abc.UnitNote
  ( defaultUnitNoteLength
  ) where

import Prelude ((/), (<))
import Data.Abc (TimeSignature, NoteDuration)
import Data.Int (toNumber)
import Data.Rational ((%))

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