{-# language QuasiQuotes #-}
{-# language ImportQualifiedPost #-}
{-# language ScopedTypeVariables #-}
module Day11 where

import AoCPrelude
import Data.Char (isAlpha)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Map.Lazy qualified as LazyMap

-------------
-- Parsing --
-------------

egInput :: String
egInput = [multi|
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
|]

type NodeID = String
data Edges = MkEdges NodeID [NodeID]
  deriving (Show, Eq)

nodeIDP :: Parser NodeID
nodeIDP = some (satisfy isAlpha)

-- >>> parseMaybe (nodeIDP) egInput
-- Just "aaa"

nodeIDsP :: Parser [NodeID]
nodeIDsP = delim' (pure ())
                  nodeIDP
                  (char ' ')
                  (newlineP <|> eof)

edgesP :: Parser Edges
edgesP = MkEdges <$> nodeIDP <* tok ":"
                 <*> nodeIDsP

inputP :: Parser [Edges]
inputP = some edgesP

-- >>> parseMaybe inputP egInput

-----------------
-- Common Task --
-----------------

parseGraph :: String -> Map NodeID [NodeID]
parseGraph
  = parseInput inputP
 .> toMapGraph

toMapGraph :: [Edges] -> Map NodeID [NodeID]
toMapGraph
  = map (\(MkEdges node nodes) -> (node, nodes))
 .> Map.fromList

-- Assume acyclic graph, otherwise there are infinite paths
numPathsBetween :: NodeID -> NodeID -> Map NodeID [NodeID] -> Int
numPathsBetween initID targetID graph = numPathsFromNode initID
  where
    numPathsFromNode :: NodeID -> Int
    numPathsFromNode nID
      | nID == "out" = 0 -- Base case: Got to "out" without finding targetID
      | targetID `elem` connectedNodes = 1
      | otherwise
          = connectedNodes                      -- For each connected node
            |> map (numPathsFromNodeMemoised !) -- Use the memoised recursive function to compute num paths
            |> sum                              -- Add together num paths from all the connected nodes
      where
        connectedNodes = graph ! nID

    -- Unsafe indexing of Map, throwing an error if the key (NodeID) isn't present
    m ! k = case m Map.!? k of
              Just ns -> ns
              _ -> error $ "NodeID (" ++ k ++ ") was not in graph. Malformed input."

    -- `numPathsFromNodeMemoised` is where the magic happens!
    -- We create a *lazy* Map from `NodeID` to `Int`,
    -- allowing us to store/memoise the number of paths from each node, without having to recompute.
    -- Effectively, we are initialising a Map of `NodeID`s to *thunks* (i.e. not-yet-evaluated values)
    -- which will evaluate to the result of `numPathsFromNode` for that node.

    -- This is more-or-less dynamic programming for 'free'/cheap

    -- Compared to imperative dynamic programming,
    -- we don't have to check if values are already in the memo table (Haskell's thunks do this for us).
    -- We also don't worry about the order we build up a memo table or threading it through our function calls.
    -- The `Map NodeID Int` and the function `NodeID -> Int` are mutually recursive
    -- and Haskell will sort out the dependency/interleaving for us.
    numPathsFromNodeMemoised :: LazyMap.Map NodeID Int
    numPathsFromNodeMemoised
      = graph
        |> Map.toAscList
        |> LazyMap.fromAscList     -- We ensure the domain of our function is in the Map by using the graph Map
        |> LazyMap.insert "out" [] -- and adding "out", since it isn't in graph but is a valid input to our memoised function
        |> LazyMap.mapWithKey (\nodeID _ -> numPathsFromNode nodeID) -- Mutual recursion with `numPathsFromNode`, creates a thunk
                                                                     -- which will be evaluated when needed

------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 11 task1


-- >>> task1 egInput
-- 5

task1 :: String -> Int
task1
  = parseInput inputP
 .> toMapGraph
 .> numPathsBetween "you" "out"

-- Original direct approach, before generalising part 1 + 2
-- pathsToOutFromNode :: NodeID -> Map NodeID [NodeID] -> Int
-- pathsToOutFromNode initID graph = go initID
--   where
--     go :: NodeID -> Int
--     go nID
--       | "out" `elem` connectedNodes = 1
--       | otherwise
--           = sum (map go connectedNodes)
--       where
--         connectedNodes
--           = case graph Map.!? nID of
--               Just ns -> ns
--               _ -> error "NodeID was not in graph. Malformed input."

------------
-- Task 2 --
------------

egInputTask2 :: String
egInputTask2 = [multi|
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
|]

runTask2 :: IO Int
runTask2 = runDay 11 task2

-- >>> task2 egInputTask2
-- 2

task2 :: String -> Int
task2 str
  = numPathsBetween "svr" "fft" g
    * numPathsBetween "fft" "dac" g
    * numPathsBetween "dac" "out" g
  + numPathsBetween "svr" "dac" g
    * numPathsBetween "dac" "fft" g
    * numPathsBetween "fft" "out" g
  where
    g = parseGraph str
