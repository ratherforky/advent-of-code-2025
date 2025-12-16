{-# language QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day5 where

import AoCPrelude -- parsing

-------------
-- Parsing --
-------------

egInput :: String
egInput = [multi|
3-5
10-14
16-20
12-18

1
5
8
11
17
32
|]

data Range = MkRange Int Int deriving (Show, Eq)
data Database = MkDB [Range] [Int] deriving (Show, Eq)


rangeP :: Parser Range
rangeP = MkRange <$> int <* char '-' <*> int

databaseP :: Parser Database
databaseP = MkDB <$> delim rangeP "\n" (string "\n\n") <*> everyLine int

-- >>> parse databaseP egInput
-- [(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8,11,17,32],""),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8,11,17],"32\n"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8,11],"17\n32\n"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8],"11\n17\n32\n"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5],"8\n11\n17\n32\n"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1],"5\n8\n11\n17\n32\n"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [],"1\n5\n8\n11\n17\n32\n")]



------------------
-- Common Task --
------------------



------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 5 task1

-- >>> task1 egInput
-- 3

task1 :: String -> Int
task1
  = parseInput databaseP
 .> numFresh


numFresh :: Database -> Int
numFresh (MkDB ranges ids)
  = ids
    |> filter (idInRanges ranges)
    |> length

idInRanges :: [Range] -> Int -> Bool
idInRanges ranges i = any (idInRange i) ranges

idInRange :: Int -> Range -> Bool
idInRange i (MkRange l r) = l <= i && i <= r

-- >>> :t any
-- any :: Foldable t => (a -> Bool) -> t a -> Bool

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 5 task2

-- >>> task2 egInput
-- 14

task2 :: String -> Int
task2
  = parseInput databaseP
 .> getRanges
 .> sortOn (\(MkRange l _) -> l)
 .> (\(MkRange l r:ranges)
       -> foldl' f (r, r - l + 1) ranges)
 .> snd
  where
    f (r1, numIds) (MkRange l2 r2)
      | r1 < l2 = (r2, numIds + r2 - l2 + 1)
      | r2 < r1 = (r1, numIds)
      | otherwise = (r2, numIds + r2 - r1)



task2Slow :: String -> Int
task2Slow
  = parseInput databaseP
 .> getRanges
 .> map expandRange
 .> concat
 .> sort
 .> group
 .> map (\(x:_) -> x)
 .> length

expandRange :: Range -> [Int]
expandRange (MkRange start end) = [start .. end]

getRanges :: Database -> [Range]
getRanges (MkDB rs _) = rs

