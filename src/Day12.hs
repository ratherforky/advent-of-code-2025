{-# language QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
module Day12 where

import AoCPrelude
import GHC.TypeLits
import Debug.Todo (todo_)

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

data Input = MkInput
  { boxes :: [Box]
  , challenges :: [PackingChallenge]
  } deriving Show

data Box = MkBox ID Shape deriving Show
type Shape = Vec 3 (Vec 3 Bool)
type ID = Int

data Vec (n :: Nat) a where
  Nil :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a
deriving instance Show a => Show (Vec n a)

data PackingChallenge = MkPC Int Int [ID] deriving Show

-- >>> parseMaybe inputP egInput

inputP :: Parser Input
inputP = MkInput <$> boxesP <*> some packingP

boxesP :: Parser [Box]
boxesP = some (boxP <* newlineP)

boxP :: Parser Box
boxP = MkBox <$> int <* tok ":"
             <*> shapeP

-- >>> parseMaybe boxP egInput
-- Just (MkBox 0 (Cons (Cons True (Cons True (Cons True Nil))) (Cons (Cons True (Cons True (Cons False Nil))) (Cons (Cons True (Cons True (Cons False Nil))) Nil))))

shapeP :: Parser Shape
shapeP = shapeLineP
   <:::> shapeLineP
   <:::> shapeLineP
   <:::> pure Nil

infixr 5 <:::>
(<:::>) :: Parser a -> Parser (Vec n a) -> Parser (Vec (n + 1) a)
px <:::> pxs = Cons <$> px <*> pxs

shapeLineP :: Parser (Vec 3 Bool)
shapeLineP = filledP <:::> filledP <:::> filledP <:::> (Nil <$ newlineP)

filledP :: Parser Bool
filledP = True <$ char '#'
      <|> False <$ char '.'

packingP :: Parser PackingChallenge
packingP
  = MkPC <$> int <* char 'x' <*> int <* tok ":"
         <*> delim int " " (newlineP <|> eof)

------------
-- Task 1 --
------------

-- runTask1 :: IO Int
runTask1 = runDay 12 task1

-- >>> task1 egInput


-- task1 :: String -> Int
task1
  = parseInput inputP
 .> challenges
--  .> filter obviouslyDontFit
 .> filter obviouslyFit
 .> length

obviouslyFit :: PackingChallenge -> Bool
obviouslyFit (MkPC w h objects)
  = sum objects <= (w `div` 3) * (h `div` 3)

obviouslyDontFit :: PackingChallenge -> Bool
obviouslyDontFit (MkPC w h objects)
  = sum objects * 9 > w * h


------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay (error "add day number") task2

-- >>> task2 egInput

task2 :: String -> Int
task2
  = error "todo"


