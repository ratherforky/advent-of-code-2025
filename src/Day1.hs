{-# language QuasiQuotes #-}
module Day1 where

import AoCPrelude -- parsing

------------
-- Task 1 --
------------

data CMD
  = L Int
  | R Int
  deriving Show

cmdP :: Parser CMD
cmdP = L <$ char 'L' <*> int
   <|> R <$ char 'R' <*> int

-- >>> parse cmdP "L68"
-- [(L 68,""),(L 6,"8")]

egInput :: String
egInput = [multi|
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
|]

-- >>> task1 egInput
-- 3

task1 :: String -> Int
task1
  = parseInput (everyLine cmdP)
 .> scanl' (\acc cmd -> case cmd of
                        L x -> (acc - x) `mod` 100
                        R x -> (acc + x) `mod` 100) 50
 .> filter (== 0)
 .> length


-- runTask1 :: IO ()
runTask1 :: IO Int
runTask1 = runDay 1 task1

------------
-- Task 2 --
------------

-- >>> task2 egInput
-- 6

task2 :: String -> Int
task2
  = parseInput (everyLine cmdP)
 .> scanl' (\(acc, _) cmd
               -> case cmd of
                    L x -> ((acc - x) `mod` 100, abs ((acc - x - 1) `div` 100 + if acc == 0 then 1 else 0) )
                    R x -> ((acc + x) `mod` 100, (acc + x) `div` 100)
           )
           (50, 0)
 .> map snd
 .> sum


runTask2 :: IO Int
runTask2 = runDay 1 task2
