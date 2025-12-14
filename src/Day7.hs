{-# language QuasiQuotes #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase #-}
module Day7 where

import AoCPrelude
import Control.Monad.Writer.Strict
import Data.Monoid (Sum(..))
import Data.Foldable (foldlM, foldrM)
import Data.Map.Strict qualified as Map

-------------
-- Parsing --
-------------

egInput :: String
egInput = [multi|
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
|]

data Cell = Empty | Spliter | Start deriving (Show, Eq)

parseCell :: Char -> Cell
parseCell = \case -- LambdaCase extension, syntax sugar for `\x -> case x of ..`
  '.' -> Empty
  '^' -> Spliter
  'S' -> Start
  _ -> error "failed to parse cell"

parseTachyonManifold :: String -> [[Cell]]
parseTachyonManifold
  = lines
 .> map (map parseCell)

-- >>> parseTachyonManifold egInput
-- [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Start,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Spliter,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Spliter,Empty,Spliter,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Spliter,Empty,Spliter,Empty,Spliter,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Spliter,Empty,Spliter,Empty,Empty,Empty,Spliter,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Spliter,Empty,Spliter,Empty,Empty,Empty,Spliter,Empty,Spliter,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Spliter,Empty,Empty,Empty,Spliter,Empty,Empty,Empty,Empty,Empty,Spliter,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Spliter,Empty,Spliter,Empty,Spliter,Empty,Spliter,Empty,Spliter,Empty,Empty,Empty,Spliter,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]


------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 7 task1

-- >>> task1 egInput
-- 21


task1 :: String -> Int
task1
  = parseTachyonManifold
 .> \case cells:cellss -> foldlM step (initBeams cells) cellss; _ -> error "No cells"
 .> execWriter
 .> getSum

initBeams :: [Cell] -> [Bool]
initBeams = map (== Start)

step :: [Bool] -> [Cell] -> Writer (Sum Int) [Bool]
step beams cells = do
  tell numSplits
  pure beams'
  where
    beams' = zipWith (||) continuingBeams splitBeams

    continuingBeams
      = zipWith (\beam spliter -> beam && not spliter)
                beams
                (map (== Spliter) cells)

    splits = zipWith f beams cells

    splitBeams = splitsToBeams splits

    numSplits = splits
                |> filter id
                |> length
                |> Sum

    f True Spliter = True
    f _    _       = False

-- >>> step [False,False,False,False,False,False,False,True,False,False,False,False,False,False,False] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Spliter,Empty,Empty,Empty,Empty,Empty,Empty,Empty]
-- WriterT (Identity ([False,False,False,False,False,False,True,False,True,False,False,False,False,False,False],Sum {getSum = 1}))

splitsToBeams :: [Bool] -> [Bool]
splitsToBeams splits
  = zipWith (||)
            (drop 1 splits ++ [False])
            (False : splits)

-- >>> splitsToBeams [False, False, True, False] == [False, True, False, True]
-- True

-- >>> splitsToBeams [False, True, False, True, False]
-- [True,False,True,False,True]

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 7 task2

-- >>> task2 egInput
-- 40

task2 :: String -> Int
task2
  = parseTachyonManifold
 .> \case cells:cellss -> foldl' stepTask2 [(initBeams cells, 1)] cellss; _ -> error "No cells"
 .> map snd
 .> sum


type NonDet a = [a] -- Using list as a non-determinism monad

stepTask2 :: NonDet ([Bool],Int) -> [Cell] -> NonDet ([Bool], Int) -- Int to record how many paths led to the same state, limiting branching factor
stepTask2 beamsNums cells = mergeStates $ do
  (beams, numPaths) <- beamsNums

  let continuingBeams
        = zipWith (\beam spliter -> beam && not spliter)
                  beams
                  (map (== Spliter) cells)

      splits = zipWith f beams cells

      f True Spliter = True
      f _    _       = False

      newSplits = splitsToManyWorldsBeams splits

  splitBeams <- newSplits

  let beams' = zipWith (||) continuingBeams splitBeams

  pure (beams', numPaths)

mergeStates :: NonDet ([Bool], Int) -> NonDet ([Bool], Int)
mergeStates = Map.fromListWith (+) .> Map.toList

-- >>> stepTask2 [([False,False,False,False,False,False,False,True,False,False,False,False,False,False,False], 1)] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Spliter,Empty,Empty,Empty,Empty,Empty,Empty,Empty]
-- [([False,False,False,False,False,False,False,False,True,False,False,False,False,False,False],1),([False,False,False,False,False,False,True,False,False,False,False,False,False,False,False],1)]

data SplitSide = L | R deriving (Show, Eq)

splitsToManyWorldsBeams :: [Bool] -> NonDet [Bool]
splitsToManyWorldsBeams
  = traverse (\split -> if split then [Just L, Just R] else [Nothing])
 .> fmap splitSidesToBeams

splitSidesToBeams :: [Maybe SplitSide] -> [Bool]
splitSidesToBeams splitSides
  = zipWith (||)
            (drop 1 (map (== Just L) splitSides) ++ [False])
            (False : map (== Just R) splitSides)
