{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-parser"
, dependencies =
  [ "bifunctors"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "midi"
  , "ordered-collections"
  , "rationals"
  , "strings"
  , "stringutils"
  , "string-parsers"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
