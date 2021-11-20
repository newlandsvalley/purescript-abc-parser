{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-parser"
, license = "MIT"
, repository = "https://github.com/newlandsvalley/purescript-abc-parser"
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
  , "profunctor-lenses"
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
