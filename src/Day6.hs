{-# language MultilineStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Day6 where

import AoCPrelude -- parsing

-------------
-- Parsing --
-------------

-- MultilineStrings extension, requires GHC 9.12.1 or above
egInput :: String
egInput = """
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""

data Op = Sum | Product deriving (Show, Eq)
data Problems = MkProblems [[Int]] [Op] deriving (Show, Eq)

opP :: Parser Op
opP = Sum     <$ tok "+"
  <|> Product <$ tok "*"

lineOfIntsP :: Parser [Int]
lineOfIntsP = delim' int space _

-- problemsP :: Parser Problems
-- problemsP = MkProblems <$> int <* char '-' <*> int

-- >>> parse databaseP egInput
-- [(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8,11,17,32],""),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8,11,17],"32"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8,11],"17\n32"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5,8],"11\n17\n32"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1,5],"8\n11\n17\n32"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [1],"5\n8\n11\n17\n32"),(MkDB [MkRange 3 5,MkRange 10 14,MkRange 16 20,MkRange 12 18] [],"1\n5\n8\n11\n17\n32")]



------------------
-- Common Task --
------------------



------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 6 task1

-- >>> task1 egInput
-- 3

task1 :: String -> Int
task1
  = undefined

------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 6 task2

-- >>> task2 egInput
-- 14

task2 :: String -> Int
task2
  = undefined