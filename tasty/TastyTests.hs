{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Lib (function1)
import TastyHspec (rootSpec)
import StatisticsTests

main :: IO ()
main = do
  s <- testSpec "hspec tests" rootSpec
  let tests = testGroup "all tests" [ statsTests, hunitTests, s ]
  defaultMain tests

hunitTests :: TestTree
hunitTests = testGroup "Unit tests" [
    testCase "broken list reverse" $ function1 [1 :: Int, 2, 3] @?= [3, 2, 1]
  ]

statsTests :: TestTree
statsTests = testProperties "Statistics"
  [ ("counter", prop_counterHasCorrectNumberOfKeys)
  , ("counter", prop_countedAllElements) ]

