{-# LANGUAGE DataKinds #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Chapter04.LinearAlgebra.ListBased
-- Copyright   :  (c) Thomas Peiselt 2018
-- License     :  Public Domain (see COPYRIGHT)
--
-- List-based implementation of the code examples from the Linear Algebra
-- Chapter of Data Science from Scratch. This is a very basic implementation,
-- staying true to the python source. Most notably, there's no encoding of
-- the length of the list in the type, so we're happily adding vectors of
-- different lengths.
-- With that limitation, it's not possible to define sensible Monoid instances,
-- because it's not possible to define the neutral element accordingly (you could
-- use [0..], but I guess that will eventually fail in one way or another).
--
-- In contrast to the python implementation, at least we can enforce the element
-- type to being numbers.
--
-- For original source code, see:
-- (https://github.com/joelgrus/data-science-from-scratch/blob/master/code/linear_algebra.py)

module Chapter04.LinearAlgebra.ListBased

  where

-- a vector is a homogenous list
type Vector a = [a]

-- | Add two vectors of the same type. Vector length is not considered.
vectorAdd :: Num a => Vector a -> Vector a -> Vector a
vectorAdd = zipWith (+)

-- | Subtract one vector from another. Vector length is not considered.
vectorSubtract :: Num a => Vector a -> Vector a -> Vector a
vectorSubtract = zipWith (-)

-- | Sum a list of vectors. Vector length is not considered, and
-- the list of vectors should be non-empty.
vectorSum :: Num a => [Vector a] -> Vector a
vectorSum = foldr1 vectorAdd

-- | Multiply a vector with a scalar.
scalarMultiply :: Num a => a -> Vector a -> Vector a
scalarMultiply scalar = fmap (scalar *)

-- | Compute the vector mean.
vectorMean :: Fractional a => Vector a -> a
vectorMean xs = sum xs / fromIntegral (length xs)

-- | Dot product of two vectors.
dot :: Num a => Vector a -> Vector a -> a
dot xs ys = sum $ zipWith (*) xs ys

sumOfSquares :: Num a => Vector a -> a
sumOfSquares xs = dot xs xs

negateVector :: Num a => Vector a -> Vector a
negateVector = map (0-)

magnitude :: Floating a => Vector a -> a
magnitude = sqrt . sumOfSquares

squaredDistance :: Floating a => Vector a -> Vector a -> a
squaredDistance xs ys = sumOfSquares $ vectorSubtract xs ys

distance :: Floating a => Vector a -> Vector a -> a
distance xs ys = sqrt $ squaredDistance xs ys

type Matrix a = [[a]]

shape :: Matrix a -> (Int, Int)
shape xs =
  let rows = length xs
  in (rows, if null xs then 0 else length (head xs))

getRow :: Int -> Matrix a -> Vector a
getRow n = (!! n)

getCol :: Int -> Matrix a -> Vector a
getCol n m = (!! n) <$> m

makeMatrix :: Int -> Int -> (Int -> Int -> a) -> Matrix a
makeMatrix r c f = [ makeRow r' | r' <- [0..r-1] ]
  where
    makeRow r' = [ f r' c' | c' <- [0..c-1] ]

showMatrix :: Show a => Matrix a -> String
showMatrix = unlines . map show

printMatrix :: Show a => Matrix a -> IO ()
printMatrix = putStrLn . showMatrix
