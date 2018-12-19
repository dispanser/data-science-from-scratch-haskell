module StatisticsTests
  ( statsSpecs
  )
where

import           Control.Exception   (evaluate)
import           Data.Complex        (Complex)
import qualified Data.HashMap.Strict as HM
import           Data.List           (nub, sort)
import           Data.Statistics
import           Test.Invariant
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Hspec

statsSpecs :: IO TestTree
statsSpecs = testSpec "Statistics module specs" $ do
  describe "mean spec"        meanSpec
  describe "median spec"      medianSpec
  describe "counter spec"     counterSpec
  describe "quantile spec"    quantileSpec
  describe "mode spec"        modeSpec
  describe "deMean spec"      deMeanSpec
  describe "variance spec"    varianceSpec
  describe "variance spec"    stdDevSpec
  describe "correlation spec" correlationSpec

meanSpec :: Spec
meanSpec = do
  it "returns 0 for empty list" $ do
    mean ([] :: [Double]) `shouldBe` 0
  it "computes mean correctly" $ do
    mean [1 :: Double, 2, 3, 4] `shouldBe` 2.5
  it "mean of single element list is element" $ property $ \x ->
    mean [x] == (x :: Double)
  it "mean is always between min and max" $ property meanInValueRange

counterSpec :: Spec
counterSpec = do
  it "number of entries in counter equals unique elements" $ do
    property $ \xs ->
      let distinct    = sort $ nub (xs :: [Int])
          counterKeys = sort $ HM.keys $ counter id xs
      in  distinct == counterKeys
  it "sum of counted values equals input size" $ do
    property countedAllElements

medianSpec :: Spec
medianSpec = do
  it "throws exception on empty list" $ do
    evaluate (median ([] :: [Double])) `shouldThrow` anyException
  it "uses average of two inner elements on list with even length"
    $          median [3 :: Rational, 8, 17, 33]
    `shouldBe` 12.5
  it "is indifferent to sorting" $ property $ \xs ->
    (not $ null xs) ==> median xs `shouldBe` median (sort (xs :: [Double]))
  it "single element list should have that median" $ property $ \x ->
    median [x] == (x :: Double)
  it "median is always within the input range" $ property medianInValueRange
  it "median and mean are the same for two-element lists" $ property $ \x y ->
    mean [x, y] == median [x, (y :: Double)]

quantileSpec :: Spec
quantileSpec = do
  it "throws exception on empty list"
    $             evaluate (quantile ([] :: [Double]) 0)
    `shouldThrow` anyException
  it "produces the only value in a singleton list"
    $ forAll (choose (0, 1))
    $ \p x -> quantile [x] (p :: Double) == (x :: Int)
  it "produces largest element for p=1" $ property $ \xs ->
    (not $ null xs) ==> quantile xs 1 == maximum (xs :: [Double])
  it "produces smarllest element for p=0" $ property $ \xs ->
    (not $ null xs) ==> quantile xs 0 == minimum (xs :: [Double])
  it "is monotonically increasing in p" $ forAll (choose (0, 1)) $ \p1 ->
    forAll (choose (0, 1)) $ \p2 xs ->
      (not $ null xs) ==> monotonicIncreasing (quantile (xs :: [Double])) p1 p2

modeSpec :: Spec
modeSpec = do
  it "finds multiple modes"
    $          mode [1, 2, 2, 4, 4, 1, 3]
    `shouldBe` [1, 2, (4 :: Int)]

deMeanSpec :: Spec
deMeanSpec = do
  it "does not change the number of elements" $ property $ \xs ->
    (not $ null xs) ==> length (deMean xs) == length (xs :: [Double])
  it "doesn't alter the data range" $ property $ \xs ->
    (not $ null xs) ==> dataRange (deMean xs) `doubleEq` dataRange
      (xs :: [Double])
  it "resulting data has mean of zero" $ property $ \xs ->
    (not $ null xs) ==> mean (deMean xs) `doubleEq` (0 :: Double)

varianceSpec :: Spec
varianceSpec = do
  it "is insensitive to shifting" $ property $ \xs y ->
    (length xs >= 2) ==> variance xs `doubleEq` variance
      ((+ (y :: Double)) <$> xs)

-- | compare double values, accepting a small absolute and relative deviation
-- Equality is assumed if the relative *or* the absolute deviation is small
doubleEq' :: Double -- ^ epsilon: maximum relative and absolute deviation
          -> Double
          -> Double
          -> Bool
doubleEq' e x y =
  let mx = max x y
      mn = min x y
  in  mx - mn < e || mx / mn < (1 + e)

-- | the double comparison used in the local tests
doubleEq :: Double -> Double -> Bool
doubleEq = doubleEq' 0.0000000001

stdDevSpec :: Spec
stdDevSpec = do
  it "is insensitive to shifting" $
    property $ \xs y -> (length xs >= 2) ==>
      stdDev xs `doubleEq` stdDev ((+ (y :: Double)) <$> xs)
  it "scales with a scaling factor" $
    property $ \xs y -> (length xs >= 2) ==>
      (y * stdDev xs) `doubleEq` (stdDev ((* (y :: Double)) <$> xs))

correlationSpec :: Spec
correlationSpec = do
  it "is 1 for self-correlation" $
    property $ \xs -> (length xs >= 2) ==>
      correlation (xs :: [Double]) xs `doubleEq` 1

countedAllElements :: [Complex Double] -> Bool
countedAllElements xs = let c = counter id xs in sum (HM.elems c) == length xs

meanInValueRange :: [Double] -> Property
meanInValueRange xs =
  length xs >=  1
    ==> let minV = minimum xs
            maxV = maximum xs
            res  = mean xs
        in  res >= minV && res <= maxV

medianInValueRange :: [Double] -> Property
medianInValueRange xs =
  length xs >=  1
    ==> let minV = minimum xs
            maxV = maximum xs
        in  median xs >= minV && median xs <= maxV

