{-# LANGUAGE OverloadedStrings #-}

import           Chapter04.LinearAlgebra.ListBasedVectorProperties (linearAlgebraTests)
import           StatisticsTests                                   (statsSpecs)
import           Test.Tasty

main :: IO ()
main = do
  statsSpecs' <- statsSpecs
  let tests = testGroup "all tests" [ statsSpecs', linearAlgebraTests ]
  defaultMain tests

currentTests :: IO ()
currentTests = statsSpecs >>= \x ->
  defaultMain $ testGroup "current development modules" [ x ]


