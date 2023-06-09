{ name = "node-env-paths"
, dependencies =
  [ "control"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "maybe"
  , "node-os"
  , "node-path"
  , "node-process"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
}
