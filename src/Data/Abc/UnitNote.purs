-- | The length of a unit note - see 3.1.7 L: - unit note length
module Data.Abc.UnitNote
        ( defaultUnitNoteLength
        ) where

import Prelude ((/), (<))
import Data.Abc (MeterSignature, NoteDuration)
import Data.Maybe (Maybe(..))  
import Data.Tuple (Tuple(..))    
import Data.Int (toNumber)
import Data.Rational ((%))  

-- caculate the default unit note length from the Meter 
-- signature (if present)
defaultUnitNoteLength :: Maybe MeterSignature -> NoteDuration
defaultUnitNoteLength mSig =
  let     
    computedMeter :: Number
    computedMeter = 
      case mSig of 
        Nothing -> 1.0  -- 4/4 is the default meter
        Just (Tuple num denom) -> (toNumber num) / (toNumber denom)
  in
    if (computedMeter < 0.75) then
      (1 % 16)
    else 
      (1 % 8)