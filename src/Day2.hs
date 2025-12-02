module Day2 where

import AoCPrelude -- parsing

-------------
-- Parsing --
-------------

egInput :: String
egInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

-- type ID = [Digit]
-- type Digit = Int
data Range = MkRange Int Int deriving (Show, Eq)

-- intDigitP :: Parser Digit
-- intDigitP = (\c -> read [c]) <$> digit

-- idP :: Parser ID
-- idP = some intDigitP

rangeP :: Parser Range
rangeP = MkRange <$> int <* char '-' <*> int

-- >>> parseMaybe rangeP "998-1012"
-- Just (MkRange 998 1012)

inputP :: Parser [Range]
inputP = delim rangeP "," (pure ())

-- >>> parseMaybe inputP egInput
-- Just [MkRange 11 22,MkRange 95 115,MkRange 998 1012,MkRange 1188511880 1188511890,MkRange 222220 222224,MkRange 1698522 1698528,MkRange 446443 446449,MkRange 38593856 38593862,MkRange 565653 565659,MkRange 824824821 824824827,MkRange 2121212118 2121212124]

------------------
-- Common Task --
------------------

task :: ([Int] -> Bool) -> String -> Int
task invalidID
  = parseInput inputP
 .> concatMap expandRange
 .> filter (getDigits .> invalidID)
 .> sum

expandRange :: Range -> [Int]
expandRange (MkRange start end) = [start .. end]

-- A fold recurses over a list.
-- An unfold generates a list!
-- For an in-depth explanation, see: https://www.youtube.com/watch?v=S_HSt6jEtWM
getDigits :: Int -> [Int]
getDigits idX = reverse (unfoldr gen idX)
  where
    gen :: Int -> Maybe (Int, Int)
    gen acc
      | acc <= 0  = Nothing
      | otherwise = Just ( acc `mod` 10 -- the digit in the 1s position
                         , acc `div` 10 -- divide by 10 so the 10s digit is now the 1s digit
                         )

-- >>> getDigits 129334
-- [1,2,9,3,3,4]

------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 2 task1

-- >>> task1 egInput
-- 1227775554

task1 :: String -> Int
task1 = task invalidIDTask1


invalidIDTask1 :: [Int] -> Bool
invalidIDTask1 digits
  | odd len   = False
  | otherwise = firstHalf == secondHalf
  where
    len = length digits
    (firstHalf, secondHalf) = splitAt (len `div` 2) digits

-- >>> invalidIDTask1 [1,1]
-- True
-- >>> invalidIDTask1 [1,1,8,8,5,1,1,8,8,5]
-- True
-- >>> invalidIDTask1 [1,1,5]
-- False


------------
-- Task 2 --
------------

runTask2 :: IO Int
runTask2 = runDay 2 task2

-- >>> task2 egInput
-- 4174379265

task2 :: String -> Int
task2 = task invalidIDTask2


invalidIDTask2 :: [Int] -> Bool
invalidIDTask2 digits
  = any allElementsEqual
        [ chunk n digits
        | n <- [1 .. length digits `div` 2]
        ]

{-
Worst case, where n is the number of digits, invalidIDTask2 is O(n^2)
because we do O(n) chunkings and for each chunking we do O(n) comparisons

In practice, laziness/shortcircuiting should help us out and make things closer to O(n).
E.g. for 123456789, chunking 1 (i.e. [[1],[2],[3],[4],[5],[6],[7],[8],[9]]), we only need to see 2 elements, [1] and [2], to determine they're not all equal and move on.
If we chunk 4 (i.e. [[1,2,3,4],[5,6,7,8],[9]]), we only need to see [1,2,3,4] and [5,6,7,8],
and we only need to see 1 and 5 to determine the lists are not equal.
-}

-- >>> invalidIDTask2 [1,1]
-- True
-- >>> invalidIDTask2 [1,1,8,8,5,1,1,8,8,5]
-- True
-- >>> invalidIDTask2 [1,1,1]
-- True
-- >>> invalidIDTask2 (getDigits 565656)
-- True


allElementsEqual :: Eq a => [a] -> Bool
allElementsEqual [] = False
allElementsEqual (x:xs) = all (x ==) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = chunkOfXs : chunk n rest
  where
    (chunkOfXs, rest) = splitAt n xs

-- >>> chunk 2 [1,1,8,8,5,1,1,8,8,5]
-- [[1,1],[8,8],[5,1],[1,8],[8,5]]
