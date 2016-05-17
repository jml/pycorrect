module Main (main) where

import BasicPrelude

import Lib (inc)

main :: IO ()
main = print . inc $ (41 :: Int)
