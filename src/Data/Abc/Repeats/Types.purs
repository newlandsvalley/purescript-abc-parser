-- | Data types representing tune sections as defined by bar 
-- | number indexes and repeat indicators which are either
-- | simple repeats or variant repeats (voltas)

module Data.Abc.Repeats.Types 
  ( Label(..)
  , Section(..)
  , Sections
  , RepeatState
  , VariantPositions ) where

import Prelude (class Eq, class Show)
import Data.Generic.Rep
import Data.Maybe (Maybe)
import Data.List (List)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

data Label =
    LeadIn     -- lead-in bars existing in the tune
  | Intro      --- artificially generated Intro
  | APart
  | OtherPart

instance showLabel :: Show Label where
  show LeadIn = "Lead-in"
  show Intro = "Intro"
  show APart = "A Part"
  show OtherPart = "Other Part"

derive instance eqLabel :: Eq Label      

-- | a map of variant number (wrt offest zero - i.e. |1 becomes 0)
-- | to the bar number where that variant is found
type VariantPositions = Map Int Int 

-- | a section of the tune (possibly repeated)
-- | with indices given by the bar number where the feature lives
newtype Section = Section
    { start :: Maybe Int
    , variantPositions :: VariantPositions
    , end :: Maybe Int
    , repeatCount :: Int 
    , label :: Label
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
    , intro :: Array Int    -- only used whenever we intend to support intros
    }  