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


------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 3 task1

-- >>> task1 egInput
-- 357

-- task1 :: String -> Int
task1
  = lines
 .> map (map (\d -> read [d] :: Int))
 .> filter (/= [])
 .> map maxBatteryVal
 .> sum

maxBatteryVal :: [Int] -> Int
maxBatteryVal [] = error "Empty line"
maxBatteryVal (x:xs) = go x xs xs
  where
    go maxDigit afterMax []  = maxDigit * 10 + maximum afterMax
    go maxDigit afterMax [_] = maxDigit * 10 + maximum afterMax
    go maxDigit afterMax [y, y']
      | y > maxDigit = y * 10 + y'
      | otherwise    = maxDigit * 10 + maximum afterMax
    go maxDigit afterMax (y:ys)
      | y > maxDigit = go y ys ys
      | otherwise    = go maxDigit afterMax ys

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
-- 4174379265

task2 :: String -> Int
task2 = undefined -- task invalidIDTask2

