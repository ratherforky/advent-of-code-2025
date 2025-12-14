{-# language QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Day4 where

import AoCPrelude -- parsing
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

-------------
-- Parsing --
-------------

egInput :: String
egInput = [multi|
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
|]

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

isAccessible :: Map (Int, Int) Cell -> Map (Int, Int) Bool
isAccessible grid = Map.mapWithKey f grid
  where
    f xy Paper = surroundingCells grid xy
                 |> filter (== Paper)
                 |> length
                 |> (< 4)
    f _  _     = False

countTrues :: Map k Bool -> Int
countTrues
  = Map.elems
 .> filter id
 .> length

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
 .> isAccessible
 .> countTrues

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 4 task2

-- >>> task2 egInput
-- 43

task2 :: String -> Int
task2 str
  = Map.intersectionWith paperMoved grid finalGrid
    |> countTrues
  where
    grid = parseCells str
    finalGrid = converge stepGrid grid

    paperMoved Paper Empty = True
    paperMoved _ _ = False


stepGrid :: Map (Int, Int) Cell -> Map (Int, Int) Cell
stepGrid grid = Map.intersectionWith stepCell boolGrid grid
  where
    boolGrid = isAccessible grid

    stepCell :: Bool -> Cell -> Cell
    stepCell True Paper = Empty
    stepCell _ c = c
