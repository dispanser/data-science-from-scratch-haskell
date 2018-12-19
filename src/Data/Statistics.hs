module Data.Statistics
  ( counter
  , mean
  , median
  , quantile
  , mode
  , dataRange
  , deMean
  , variance
  , stdDev
  , interquartileRange
  , covariance
  , correlation
  )
where

import           Chapter04.LinearAlgebra.ListBased
                                                ( dot
                                                , sumOfSquares
                                                )
import           Data.Hashable
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( sort )

counter :: (Eq b, Hashable b) => (a -> b) -> [a] -> HashMap b Int
counter f = foldr (\a -> HM.insertWith (+) (f a) 1) HM.empty

mean :: (Fractional a, Traversable t) => t a -> a
mean xs | null xs   = 0
        | otherwise = sum xs / (fromIntegral $ length xs)

median :: (Fractional a, Ord a) => [a] -> a
median xs =
  let s = sort xs
      l = length xs
      m = l `div` 2
  in  if even l then ((s !! m) + (s !! (m - 1))) / 2 else s !! m

quantile :: (Num a, Ord a) => [a] -> Double -> a
quantile xs p =
  let s = sort xs
      l = pred $ length xs
  in  s !! (floor $ p * (fromIntegral l))

mode :: (Num a, Eq a, Hashable a) => [a] -> [a]
mode xs =
  let counts = counter id xs
      maxVal = maximum $ HM.elems counts
      elems  = HM.filter (== maxVal) counts
  in  fst <$> HM.toList elems

-- | compute the range (@data_range@) in the book.
-- similar to the book implementation, we pass over the data twice
dataRange :: (Num a, Ord a) => [a] -> a
dataRange xs = maximum xs - minimum xs

deMean :: Fractional a => [a] -> [a]
deMean xs = let m = mean xs in (subtract m) <$> xs

variance :: Fractional a => [a] -> a
variance xs = (sumOfSquares $ deMean xs) / (fromIntegral $ length xs - 1)

stdDev :: Floating a => [a] -> a
stdDev = sqrt . variance

interquartileRange :: (Num a, Ord a) => [a] -> a
interquartileRange xs = quantile xs 0.75 - quantile xs 0.25

covariance :: Fractional a => [a] -> [a] -> a
covariance xs ys = dot (deMean xs) (deMean ys) / (fromIntegral $ length xs - 1)

correlation :: (Floating a, Ord a) => [a] -> [a] -> a
correlation x y =
  let stdDevX = stdDev x
      stdDevY = stdDev y
  in  if stdDevX > 0 && stdDevY > 0
        then covariance x y / stdDevX / stdDevY
        else 0
