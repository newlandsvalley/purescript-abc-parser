{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-parser"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "midi"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "rationals"
  , "string-parsers"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
