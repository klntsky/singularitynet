{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "math"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "rationals"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
