module Test.Utils where

import Prelude (($), map)
import Data.Either (Either(..))
import Data.Abc.Parser (parse)
import Abc (AbcTune)
import Data.Abc.Canonical (fromTune)

import Test.Unit (Test, failure)
import Test.Unit.Assert as Assert

{- assert the moved parsed input equals the target -}
assertMoveMatches :: forall e. String -> (AbcTune -> AbcTune) -> String -> Test e
assertMoveMatches s move target =
    let
        movedResult =
          map move $ parse s
            -- mapError (\x -> "parse error: " ++ toString x) (parse s)
    in
        case movedResult of
            Right res ->
                Assert.equal target (fromTune res)

            Left errs ->
                failure "unexpected error"

{- assert the value of some Int producing function on a parsed tune -}
assertIntFuncMatches :: forall e .String -> (AbcTune -> Int) -> Int -> Test e
assertIntFuncMatches s f target =
    let
        result =
          map f $ parse s
            -- mapError (\x -> "parse error: " ++ toString x) (parse s)
    in
        case result of
            Right res ->
                Assert.equal target res

            Left errs ->
                failure "unexpected error"
