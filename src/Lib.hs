{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
module Lib
  ( function1
  , fun1debug
  )
where

import Debug

-- broken reverse function that doesn't work on singleton lists
function1 :: [a] -> [a]
function1 [_] = []
function1 xs  = reverse xs

debug [d|
  fun1debug :: [a] -> [a]
  fun1debug []     = []
  fun1debug (x:xs) = fun1debug xs ++ [x]
  |]
