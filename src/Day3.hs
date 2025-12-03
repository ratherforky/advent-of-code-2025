{-# language MultilineStrings #-}
module Day3 where

import AoCPrelude -- parsing

-------------
-- Parsing --
-------------

-- MultilineStrings extension, requires GHC 9.12.1 or above
egInput :: String
egInput = """
987654321111111
811111111111119
234234234234278
818181911112111
"""

------------------
-- Common Task --
------------------

task :: Int -> String -> Int
task numDigits
  = lines
 .> map (map (\d -> read [d] :: Int))
 .> filter (/= [])
 .> map (maxBatteryValN numDigits)
 .> sum

maxBatteryValN :: Int -> [Int] -> Int
maxBatteryValN n = maxBatteryDigitsN n .> digitsToInt

maxBatteryDigitsN :: Int -> [Int] -> [Int]
maxBatteryDigitsN _ [] = []
maxBatteryDigitsN n ds
  | n <= 0 = []
  | otherwise = tails ds
             |> take (length ds - n + 1)
             |> foldl' (\(maxVal, rest) (d:ds)
                           -> if d > maxVal
                             then (d, ds)
                             else (maxVal, rest))
                       (-1, [])
             |> (\(maxVal, rest) -> maxVal : maxBatteryDigitsN (n - 1) rest)

-- >>> maxBatteryDigitsN 5 [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]
-- [9,8,7,6,5]
-- >>> maxBatteryDigitsN 5 [2,3,4,2,3,4,2,3,4,2,3,4,2,7,8]
-- [4,4,4,7,8]

digitsToInt :: [Int] -> Int
digitsToInt
  = reverse
 .> zipWith (*) [10 ^ n | n <- [0 :: Int ..]]
 .> sum

-- >>> digitsToInt [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]
-- 987654321111111

-- go initN x xs xs
  -- where
  --   go n maxDigit afterMax []  = maxDigit * 10 + maximum afterMax
  --   go n maxDigit afterMax [_] = maxDigit * 10 + maximum afterMax
  --   go n maxDigit afterMax [y, y']
  --     | y > maxDigit = y * 10 + y'
  --     | otherwise    = maxDigit * 10 + maximum afterMax
  --   go maxDigit afterMax (y:ys)
  --     | y > maxDigit = go y ys ys
  --     | otherwise    = go maxDigit afterMax ys

------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 3 task1

-- >>> task1 egInput
-- 357

task1 :: String -> Int
task1 = task 2

-- Original solution for task 1:

-- task1 :: String -> Int
-- task1
--   = lines
--  .> map (map (\d -> read [d] :: Int))
--  .> filter (/= [])
--  .> map maxBatteryVal
--  .> sum

-- maxBatteryVal :: [Int] -> Int
-- maxBatteryVal [] = error "Empty line"
-- maxBatteryVal (x:xs) = go x xs xs
--   where
--     go maxDigit afterMax []  = maxDigit * 10 + maximum afterMax
--     go maxDigit afterMax [_] = maxDigit * 10 + maximum afterMax
--     go maxDigit afterMax [y, y']
--       | y > maxDigit = y * 10 + y'
--       | otherwise    = maxDigit * 10 + maximum afterMax
--     go maxDigit afterMax (y:ys)
--       | y > maxDigit = go y ys ys
--       | otherwise    = go maxDigit afterMax ys

-- >>> maxBatteryVal [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]
-- 98
-- >>> maxBatteryVal [8,1,8,1,8,1,9,1,1,1,1,2,1,1,1]
-- 92

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 3 task2

-- >>> task2 egInput
-- 3121910778619

task2 :: String -> Int
task2 = task 12

