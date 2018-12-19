module Diagrams.Graphs

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Graphs
-- Copyright   :  (c) Thomas Peiselt 2018
-- License     :  Public Domain (see COPYRIGHT)
--
-- diagrams

  where

import           Data.List                    (foldl')
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

-- | draw a ring graph with `n` elements
ringGraph :: Int            -- ^ number of vertices
          -> (Int -> Int -> Bool) -- ^ directed edge predicate
          -> Diagram B            -- ^ resulting ring graph diagram
ringGraph n edgeP =
  let poly   = regPoly n 1
      node :: Int -> Diagram B
      node i = text (show i ) # fontSizeL 0.2 # fc white
             <> circle 0.15 # fc green # named i
      edges = [(v1, v2) | v1 <- [1 .. n], v2 <- [1..n], edgeP v1 v2]
      edgeF = foldl' (.) id $ map (uncurry connectOutside) edges
  in atPoints (trailVertices poly) (map node [1..]) # edgeF

-- gridGraph :: Int
--           -> Int
--           -> (Int -> Int -> Bool)
--           -> Diagram B
-- gridGraph r c edgeP = undefined

-- edges = [(3, 4), (7, 1), (1, 2)] :: [(Int, Int)]
-- d = ringGraph 8 (\x y -> (x, y) `elem` edges)
-- diagram d

-- names d -- print all the existing names in a diagram
