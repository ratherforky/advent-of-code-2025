{-# language QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Day6 where

import AoCPrelude -- parsing

-------------
-- Parsing --
-------------

egInput :: String
egInput = [multi|
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
|]

data Op = Sum | Product deriving (Show, Eq)
data Problems = MkProblems [[Int]] [Op] deriving (Show, Eq)

opP :: Parser Op
opP = Sum     <$ tok "+"
  <|> Product <$ tok "*"

lineOfIntsP :: Parser [Int]
lineOfIntsP
  = delim' (many (char ' '))              -- start
           int                            -- elements
           (some (char ' '))              -- deliminator
           (many (char ' ') <* char '\n') -- end
  -- = some (many (char ' ') *> int) <* (many (char ' ') <* char '\n')

problemsP :: Parser Problems
problemsP = MkProblems <$> some lineOfIntsP <*> some opP

-- >>> parseMaybe problemsP egInput
-- Just (MkProblems [[123,328,51,64],[45,64,387,23],[6,98,215,314]] [Product,Sum,Product,Sum])


------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 6 task1

-- >>> task1 egInput
-- 4277556

task1 :: String -> Int
task1
  = parseInput problemsP
 .> task1Problem

task1Problem :: Problems -> Int
task1Problem (MkProblems nss ops)
  = transpose nss
    |> zipWith (\case Sum     -> sum
                      Product -> product)
              ops
    |> sum

------------
-- Task 2 --
------------

cephalopodEquationsP :: Parser [(Op, [Int])]
cephalopodEquationsP
  = some (cephalopodEquationP <* whitespace)

cephalopodEquationP :: Parser (Op, [Int])
cephalopodEquationP = do
  _ <- spaces
  x <- int
  _ <- spaces
  op <- opP
  xs <- some (spaces *> int <* spaces <* char '\n')
  pure (op, x:xs)

egInputTransposed :: String
egInputTransposed = "1  *\n24  \n356 \n    \n369+\n248 \n8   \n    \n 32*\n581 \n175 \n    \n623+\n431 \n  4 \n"

-- >>> parseMaybe (some (cephalopodEquationP <* whitespace)) egInputTransposed
-- Just [(Product,[1,24,356]),(Sum,[369,248,8]),(Product,[32,581,175]),(Sum,[623,431,4])]

runTask2 :: IO Int
runTask2 = runDay 6 task2

-- >>> task2 egInput
-- 3263827

task2 :: String -> Int
task2
  = lines
 .> transpose
 .> unlines
 .> parseInput cephalopodEquationsP
 .> map ((\case (Sum, xs)     -> sum xs
                (Product, xs) -> product xs))
 .> sum


