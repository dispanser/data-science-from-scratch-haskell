{-# LANGUAGE OverloadedStrings #-}

import           Chapter04.LinearAlgebra.ListBasedVectorProperties
import           Data.Complex                                      (Complex)
import           Lib                                               (function1)
import           StatisticsTests
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = do
  let tests = testGroup "all tests" [ statsTests, hunitTests, linearAlgebraTests ]
  defaultMain tests

currentTests :: IO ()
currentTests = defaultMain $ testGroup "current development modules" [ linearAlgebraTests ]

statsTests :: TestTree
statsTests = testProperties "Statistics"
  [ ("counter", prop_counterHasCorrectNumberOfKeys)
  , ("counter", prop_countedAllElements) ]

linearAlgebraTests :: TestTree
linearAlgebraTests = testProperties "Linear Algebra"
  [ ("addition of zero", property (plusZero :: [Double] -> Bool))
  , ("commutative add", property (commutativeAdd :: [Double] -> [Double] -> Bool))
  -- associativity doesn't hold for double due to lack of numeric precision
  , ("associative add", property (associativeAdd :: [Int] -> [Int] -> [Int] -> Bool))
  , ("subtraction of zero", property (minusZero :: [Double] -> Bool))
  , ("subtraction from itself", property (minusItself :: [Integer] -> Bool))
  , ("subtraction inverts addition", property (subtractionInvertsAddition :: [Rational] -> [Rational] -> Property))
  , ("vector sum two", property (sum2 :: [Double] -> [Double] -> Bool))
  , ("vector sum three", property (sum3 :: [Double] -> [Double] -> [Double] -> Bool))
  , ("times zero", property (timesZero :: [Double] -> Bool))
  , ("times one", property (timesOne :: [Double] -> Bool))
  , ("times two", property (timesTwo :: [Complex Double]  -> Bool))
  , ("mean singleton", property (singletonMean :: Complex Double -> Bool))
  , ("magnitude is not negative", property (magnitudeNonNegative :: [Double] -> Bool))
  , ("negate . negate does nothing", property (involutoryNegate :: [Double] -> Bool))
  , ("mean of negated", property meanOfNegated)
  , ("distance to self", property (distanceToSelf :: [Double] -> Property))
  , ("distance to zero", property (distanceToZero :: [Double] -> Property))
  ]
