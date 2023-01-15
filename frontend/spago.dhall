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
  , "control"
  , "datetime"
  , "exceptions"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "math"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "ordered-collections"
  , "optparse"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "record"
  , "strings"
  , "spec"
  , "transformers"
  , "tuples"
  , "uint"
  , "unfoldable"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
