module StatisticsTests
  ( prop_counterHasCorrectNumberOfKeys
  , prop_countedAllElements )

where

import qualified Data.HashMap.Strict as HM
import Test.QuickCheck
import Data.List (nub, sort)
import Data.Statistics (counter)

counterHasCorrectNumberOfKeys :: [Int] -> Bool
counterHasCorrectNumberOfKeys xs =
  let distinct    = sort $ nub xs
      counterKeys = sort $ HM.keys $ counter id xs
  in distinct == counterKeys

prop_counterHasCorrectNumberOfKeys :: Property
prop_counterHasCorrectNumberOfKeys = property counterHasCorrectNumberOfKeys

countedAllElements :: [Int] -> Bool
countedAllElements xs =
  let c = counter id xs
  in sum (HM.elems c) == length xs

prop_countedAllElements :: Property
prop_countedAllElements = property countedAllElements
