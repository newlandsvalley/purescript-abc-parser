module Data.Abc.Normaliser
  ( normalise
  , normaliseBrokenRhythm
  , normaliseChord
  , normaliseTuneBody
  ) where

import Data.Abc (AbcTune, AbcChord, TuneBody, BodyPart(..), Bar, Broken(..), Music(..), RestOrNote)
import Prelude

import Data.Abc.Utils (dotFactor)
import Data.Either (Either(..))
import Data.List (List(..), foldr, (:))
import Data.Rational (fromInt, toNumber, (%))
import Data.Tuple (Tuple(..))

-- | normalise the tune by flattening any broken rhythm pairs and regularising chord representations
normalise :: AbcTune -> AbcTune
normalise t =
  { headers: t.headers, body: (normaliseTuneBody t.body) }

-- | as for normalise but just applied to the tune body
normaliseTuneBody :: TuneBody -> TuneBody
normaliseTuneBody =
  map normaliseBodyPart

normaliseBodyPart :: BodyPart -> BodyPart
normaliseBodyPart bp =
  case bp of
    Score ms ->
      Score (normaliseBarList ms)
    _ ->
      bp

normaliseBarList :: List Bar -> List Bar
normaliseBarList =
  map normaliseBar
  
normaliseBar :: Bar -> Bar
normaliseBar bar =
  let
    newMusic =
      foldr normaliseMusic Nil bar.music
  in
    bar { music = newMusic }

normaliseMusic ::  Music -> List Music -> List Music
normaliseMusic next acc = 
  case next of
    BrokenRhythmPair operand1 operator operand2 ->
      let 
        (Tuple music1 music2) = normaliseBrokenRhythm operator operand1 operand2
      in
        music1 : (music2 : acc)

    Chord c ->
      Chord (normaliseChord c) : acc

    _ -> 
      next : acc 

-- | Apply the specified broken rhythm to each note in the note pair (presented individually)
-- | and return the broken note pair simply as a pair of normalised Music items held in a Tuple
normaliseBrokenRhythm :: Broken -> RestOrNote -> RestOrNote -> (Tuple Music Music)
normaliseBrokenRhythm broken rorNa rorNb =
  let 
    factora = 
      case broken of 
        LeftArrow i -> 
          (fromInt 1) - (dotFactor i)
        RightArrow i -> 
          (fromInt 1) + (dotFactor i)
    factorb = 
      case broken of 
        LeftArrow i -> 
          (fromInt 1) + (dotFactor i)
        RightArrow i -> 
          (fromInt 1) - (dotFactor i)
    musica =
      case rorNa of 
        Left r -> 
          Rest r { duration = r.duration * factora}
        Right gn ->           
          let 
            newAbcNote = gn.abcNote { duration = gn.abcNote.duration * factora }
          in 
            Note gn { abcNote = newAbcNote }
    musicb =
      case rorNb of 
        Left r -> 
          Rest r  { duration = r.duration * factorb}
        Right gn ->   
          let 
            newAbcNote = gn.abcNote { duration = gn.abcNote.duration * factorb }
          in
            Note gn { abcNote = newAbcNote }
  in 
    Tuple musica musicb

-- | Normalise an ABC chord by placing the correct duration against each note
-- | and setting the overall Chord length to Unit
normaliseChord :: AbcChord -> AbcChord
normaliseChord abcChord =
  case (toNumber abcChord.duration) of
    1.0 -> abcChord
    _ ->
      let
        notes = map (\n -> n { duration = n.duration * abcChord.duration }) abcChord.notes
        decorations = abcChord.decorations
        leftSlurs = abcChord.leftSlurs
        rightSlurs = abcChord.rightSlurs
      in
        { leftSlurs, decorations, notes, duration: (1 % 1), rightSlurs }



