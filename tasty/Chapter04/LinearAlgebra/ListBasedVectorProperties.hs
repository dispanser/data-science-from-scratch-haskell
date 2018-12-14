----------------------------------------------------------------------------
-- |
-- Module      :  ListBasedVectorProperties
-- Copyright   :  (c) Thomas Peiselt 2018
-- License     :  Public Domain (see COPYRIGHT)
--
-- property-based tests for the list-based implementation of the linear algebra
-- packages.
module Chapter04.LinearAlgebra.ListBasedVectorProperties (
        linearAlgebraTests )

  where

import           Chapter04.LinearAlgebra.ListBased
import           Data.Complex                      (Complex)
import           Test.Invariant
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

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

plusZero :: (Num a, Eq a) => Vector a -> Bool
plusZero xs = vectorAdd xs (repeat 0) == xs

commutativeAdd :: (Num a, Eq a) => Vector a -> Vector a -> Bool
commutativeAdd = commutative vectorAdd

associativeAdd :: (Num a, Eq a) => Vector a -> Vector a -> Vector a -> Bool
associativeAdd = associative vectorAdd

minusZero :: (Num a, Eq a) => Vector a -> Bool
minusZero xs =  vectorSubtract xs (repeat 0) == xs

minusItself :: (Num a, Eq a) => Vector a -> Bool
minusItself xs = vectorSubtract xs xs == (replicate (length xs) 0)

subtractionInvertsAddition :: (Num a, Eq a) => Vector a -> Vector a -> Property
subtractionInvertsAddition xs ys = length xs <= length ys ==>
  inverts (`vectorAdd` ys) (`vectorSubtract` ys) xs

sum2 :: (Num a, Eq a) => Vector a -> Vector a -> Bool
sum2 xs ys = vectorSum [xs, ys] == vectorAdd xs ys

sum3 :: (Num a, Eq a) => Vector a -> Vector a -> Vector a -> Bool
sum3 xs ys zs = vectorSum [xs, ys, zs] == vectorAdd xs ( vectorAdd ys zs )

timesZero :: (Num a, Eq a) => Vector a -> Bool
timesZero xs = scalarMultiply 0 xs == replicate (length xs) 0

timesOne :: (Num a, Eq a) => Vector a -> Bool
timesOne xs = scalarMultiply 1 xs == xs

timesTwo :: (Num a, Eq a) => Vector a -> Bool
timesTwo xs = scalarMultiply 2 xs == vectorAdd xs xs

singletonMean :: (Fractional a, Eq a) => a -> Bool
singletonMean x = vectorMean [x] == x

involutoryNegate :: (Eq a, Num a) => Vector a -> Bool
involutoryNegate = involutory negateVector

meanOfNegated :: [Double] -> Property
meanOfNegated xs =  not (null xs) ==> vectorMean xs == -(vectorMean $ negateVector xs)

magnitudeNonNegative :: (Floating a, Eq a, Ord a) => Vector a -> Bool
magnitudeNonNegative xs = magnitude xs >= 0.0

distanceToSelf :: (Floating a, Eq a) => Vector a -> Property
distanceToSelf xs = not (null xs) ==> distance xs xs == 0

distanceToZero :: (Floating a, Eq a, Enum a) => Vector a -> Property
distanceToZero xs = not (null xs) ==>
  distance xs (replicate (length xs) 0) == magnitude xs
