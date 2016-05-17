module Main (main) where

import BasicPrelude

import Criterion
import Criterion.Main

import PyCorrect (inc)

main :: IO ()
main = defaultMain [bench "inc 41" (whnf inc (41 :: Int))]
