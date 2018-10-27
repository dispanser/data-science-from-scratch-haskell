module Main where

import           Criterion.Main
import           Lib            (function1)

main :: IO ()
main = defaultMain
  [ bgroup "group one"
           [ bench "reverse all the things" $ whnf function1 [1 :: Int .. 10]]
  ]
