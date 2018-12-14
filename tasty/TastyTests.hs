{-# LANGUAGE OverloadedStrings #-}

import           Chapter04.LinearAlgebra.ListBasedVectorProperties (linearAlgebraTests)
import           StatisticsTests
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = do
  let tests = testGroup "all tests" [ statsTests, linearAlgebraTests ]
  defaultMain tests

currentTests :: IO ()
currentTests = defaultMain $ testGroup "current development modules" [ linearAlgebraTests ]

statsTests :: TestTree
statsTests = testProperties "Statistics"
  [ ("counter", prop_counterHasCorrectNumberOfKeys)
  , ("counter", prop_countedAllElements) ]


