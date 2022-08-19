module Data.Abc.Meter 
  ( getMeter
  , getDefaultedMeter
  , commonTime
  , cutTime
  , toRational
  ) where

import Prelude (($), (<<<), join)
import Data.Abc (AbcTune, TimeSignature)
import Data.Abc.Optics (_headers, _Meter)
import Data.Maybe (Maybe, fromMaybe)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
import Data.Rational (Rational, (%))

-- | Get the tune Meter where present
-- | For more flexibility, you should use the _Meter optic.
getMeter :: AbcTune -> Maybe TimeSignature
getMeter tune =
  join $ (firstOf (_headers <<< traversed <<< _Meter) tune)

-- | Get the meter defaulting to 4/4
getDefaultedMeter :: AbcTune -> TimeSignature
getDefaultedMeter tune =
  fromMaybe commonTime $ getMeter tune

-- | common time - 4/4
commonTime :: TimeSignature 
commonTime =
  { numerator: 4, denominator: 4}

-- | cut time - 2/2
cutTime :: TimeSignature 
cutTime =
  { numerator: 2, denominator: 2}

-- | convert the time signature to a Rational
toRational :: TimeSignature -> Rational 
toRational ts = 
  ( ts.numerator % ts.denominator )

