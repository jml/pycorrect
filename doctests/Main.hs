-- | Run the doctests for the project.
module Main (main) where

import BasicPrelude

import Test.DocTest


languageOptions :: [String]
languageOptions =
  [ "NoImplicitPrelude"
  , "OverloadedStrings"
  , "NamedFieldPuns"
  , "RecordWildCards"
  , "GeneralizedNewtypeDeriving"
  ]


main :: IO ()
main = doctest $ ghcFlags ++ ["lib/"]
  where
    ghcFlags = [ "-X" ++ option | option <- languageOptions ]
