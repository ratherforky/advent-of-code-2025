{-# language MultilineStrings #-}
module DayTemplate where

import AoCPrelude

-------------
-- Parsing --
-------------

-- MultilineStrings extension, requires GHC 9.12.1 or above
egInput :: String
egInput = """

"""


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


