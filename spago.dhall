{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "prettier-printer"
, dependencies =
    [ "console"
    , "lists"
    , "prelude"
    , "psci-support"
    , "quickcheck"
    , "spec"
    , "spec-discovery"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
