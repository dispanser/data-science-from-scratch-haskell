{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit
import Lib (function1)
import TastyHspec (rootSpec)

main :: IO ()
main = do
  s <- testSpec "hspec tests" rootSpec
  let tests = testGroup "all tests" [ hunitTests, s ]
  defaultMain tests

hunitTests :: TestTree
hunitTests = testGroup "Unit tests" [
    testCase "broken list reverse" $ function1 [1 :: Int, 2, 3] @?= [3, 2, 1]
  ]





