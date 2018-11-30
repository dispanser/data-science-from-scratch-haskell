module Data.Statistics
  ( counter
  )

  where

import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- TODO: use unordered containers and Data.HashMap.Strict for performance
-- simple reproduction of the `Counter` python class (from `collections`)
counter :: (Eq b, Hashable b) => (a -> b) -> [a] -> HashMap b Int
counter f = foldr (\a -> HM.insertWith (+) (f a) 1) HM.empty



