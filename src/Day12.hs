{-# language QuasiQuotes #-}
module Day12 where

import AoCPrelude

-------------
-- Parsing --
-------------

egInput :: String
egInput = [multi|
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
|]


------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay (error "add day number") task1

-- >>> task1 egInput


task1 :: String -> Int
task1
  = error "todo"

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay (error "add day number") task2

-- >>> task2 egInput

task2 :: String -> Int
task2
  = error "todo"


