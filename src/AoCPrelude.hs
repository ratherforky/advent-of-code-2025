{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module AoCPrelude (
  runDay,
  module Data.List,
  -- module Data.String.Interpolate,
  multi, -- QuasiQuoter
  module Flow,
  -- Parsing
  module Text.Yoda,
  parseInput, digit, int, intSigned, integral,
  everyLine, manyIgnoreWhitespace, tok, delim, delim',
  whitespace, spaces,
  -- module Text.Pretty.Simple,
  void,
  -- Misc
  applyN, applyAll, applyNScan,
  scanUntilNothing,
  within, betweenOrd, firstJusts,
  converge
  ) where

import Data.List
import Data.String.Interpolate
import Flow
-- import Text.Megaparsec hiding (State)
-- import Text.Megaparsec.Char
import Text.Yoda
import Control.Monad (void)
import Data.Maybe
-- import Text.Pretty.Simple
import Data.Foldable
import Data.Char

-- Renaming `i` QuasiQuoter to `multi` to avoid name conflicts
multi = i

runDay :: Int -> (String -> b) -> IO b
runDay day task = task <$> readFile [i|src/inputs/Day#{day}Input.txt|]

parseInput :: Parser a -> String -> a
parseInput parser = parseMaybe parser .> fromJust

everyLine :: Parser a -> Parser [a]
everyLine parser = many (parser <* (void newline <|> eof))

newline :: Parser Char
newline = char '\n'

manyIgnoreWhitespace :: Parser a -> Parser [a]
manyIgnoreWhitespace parser = many (parser <* (whitespace <|> eof))

whitespace :: Parser ()
whitespace = () <$ many (satisfy isSpace)

spaces :: Parser ()
spaces = () <$ many (char ' ')

int :: Parser Int
int = read <$> some digit

digit :: Parser Char
digit = satisfy isDigit

integral :: (Integral a, Read a) => Parser a
integral = read <$> some digit


intSigned :: Parser Int
intSigned
  = choice
      [ char '-' *> (negate <$> int)
      , int
      ]

tok :: String -> Parser ()
tok str = whitespace *> string str *> whitespace

delim :: Parser a -> String -> Parser c -> Parser [a]
delim elementP delimiter terminatorP
  = delim' (pure ()) elementP (tok delimiter) terminatorP

-- >>> parse (delim digit "," eof) "4,5"
-- [("45","")]

delim' :: Parser start -> Parser a -> Parser delim -> Parser end -> Parser [a]
delim' startP elementP delimiterP terminatorP = do
  _ <- startP
  elements <- many (elementP <* delimiterP)
  lastElement <- elementP
  _ <- terminatorP
  pure (elements ++ [lastElement])


applyN :: Int -> (a -> a) -> a -> a
applyN n0 f = go n0
  where
    go 0 a = a
    go n a = go (n-1) (f a)

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl' (flip ($)) x fs

applyNScan :: Int -> (a -> a) -> a -> [a]
applyNScan n0 f = go n0
  where
    go 0 a = [a]
    go n a = a : go (n-1) (f a)

scanUntilNothing :: (a -> Maybe a) -> a -> [a]
scanUntilNothing f = go
  where
    go x = case f x of
      Nothing -> [x]
      Just x' -> x : go x'


within :: Int -> Int -> Int -> Bool
within n x y = abs (x - y) <= n

betweenOrd :: Ord a => a -> a -> a -> Bool
betweenOrd maxX minX x = maxX >= x && x >= minX

-- From GHC.Data.Maybe
-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: Foldable f => f (Maybe a) -> Maybe a
firstJusts = msum
{-# SPECIALISE firstJusts :: [Maybe a] -> Maybe a #-}

converge :: Eq a => (a -> a) -> a -> a
converge f k = go $ iterate f k
  where
    go (x:y:ys)
      | x == y    = x
      | otherwise = go (y:ys)
    go _ = error "iterate is infinite, so this should never happen"