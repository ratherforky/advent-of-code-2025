{-# language MultilineStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Day4 where

import AoCPrelude -- parsing
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

-------------
-- Parsing --
-------------

-- MultilineStrings extension, requires GHC 9.12.1 or above
egInput :: String
egInput = """
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"""

data Cell = Empty | Paper
  deriving (Show, Eq)

toCell :: Char -> Cell
toCell '.' = Empty
toCell '@' = Paper
toCell c = error $ "Invalid character: " ++ show c

parseCells :: String -> Map (Int, Int) Cell
parseCells
  = lines
 .> map (map toCell)
 .> flip zipWith [0 :: Int ..] (\y ->
      flip zipWith [0 :: Int ..] (\x c -> ((x,y), c)))
 .> concat
 .> Map.fromList

------------------
-- Common Task --
------------------

kernel :: [(Int, Int)]
kernel = [(x,y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0)]

-- >>> kernel
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

surroundingCells :: Map (Int, Int) Cell -> (Int,Int) -> [Cell]
surroundingCells grid (x,y)
  = catMaybes [ grid Map.!? (x + dx, y + dy)
              | (dx, dy) <- kernel
              ]

mapPredicate :: (Cell -> [Cell] -> Bool) -> Map (Int, Int) Cell -> Map (Int, Int) Bool
mapPredicate p grid
  = grid
    |> Map.mapWithKey (\xy c -> p c (surroundingCells grid xy))

------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 4 task1

-- >>> task1 egInput
-- 13

task1 :: String -> Int
task1
  = parseCells
 .> mapPredicate task1Predicate
 .> Map.elems
 .> filter id
 .> length

task1Predicate :: Cell -> [Cell] -> Bool
task1Predicate Paper = filter (== Paper) .> length .> (< 4)
task1Predicate _     = const False

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 4 task2

-- >>> task2 egInput
-- 3121910778619

task2 :: String -> Int
task2 = undefined

