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
  , "arraybuffer-types"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "console"
  , "datetime"
  , "exceptions"
  , "effect"
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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
