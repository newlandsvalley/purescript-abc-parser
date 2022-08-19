module Test.Utils where

import Prelude (Unit, ($), map)
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Abc.Parser (parse)
import Data.Abc (Accidental, AbcTune, Mode, ModifiedKeySignature, PitchClass)
import Data.Abc.Canonical (fromTune)
import Data.List (List(..))
import Data.Map (empty)

import Test.Spec.Assertions (fail, shouldEqual)

{- assert the moved parsed input equals the target -}
assertMoveMatches :: String -> (AbcTune -> AbcTune) -> String -> Aff Unit
assertMoveMatches s move target =
  let
    movedResult =
      map move $ parse s
  -- mapError (\x -> "parse error: " ++ toString x) (parse s)
  in
    case movedResult of
      Right res ->
        target `shouldEqual` (fromTune res)

      Left _errs ->
        fail "unexpected error"

{- assert the value of some Int producing function on a parsed tune -}
assertIntFuncMatches :: String -> (AbcTune -> Int) -> Int -> Aff Unit
assertIntFuncMatches s f target =
  let
    result =
      map f $ parse s
  -- mapError (\x -> "parse error: " ++ toString x) (parse s)
  in
    case result of
      Right res ->
        target `shouldEqual` res

      Left _errs ->
        fail "unexpected error"


buildKeySig :: PitchClass -> Accidental -> Mode -> ModifiedKeySignature
buildKeySig pc acc mode =
  { keySignature: { pitchClass: pc, accidental: acc, mode: mode }
  , modifications: Nil
  , properties: empty
  }          
