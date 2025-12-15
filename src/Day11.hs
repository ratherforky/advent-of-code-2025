{-# language QuasiQuotes #-}
{-# language ImportQualifiedPost #-}
module Day11 where

import AoCPrelude
import Data.Char (isAlpha)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Map.Lazy qualified as LazyMap
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Todo (todo_)
import Data.Foldable (traverse_)

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

-- data NodeID = You | Out | ID String
--   deriving (Show, Eq, Ord)
type NodeID = String
data Edges = MkEdges NodeID [NodeID]
  deriving (Show, Eq)

nodeIDP :: Parser NodeID
nodeIDP = some (satisfy isAlpha)
-- nodeIDP = You <$ string "you"
--       <|> Out <$ string "out"
--       <|> ID <$> some (satisfy isAlpha)

-- >>> parseMaybe (nodeIDP) egInput
-- Just (ID "aaa")

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
-- Just [MkEdges (ID "aaa") [You,ID "hhh"],MkEdges You [ID "bbb",ID "ccc"],MkEdges (ID "bbb") [ID "ddd",ID "eee"],MkEdges (ID "ccc") [ID "ddd",ID "eee",ID "fff"],MkEdges (ID "ddd") [ID "ggg"],MkEdges (ID "eee") [Out],MkEdges (ID "fff") [Out],MkEdges (ID "ggg") [Out],MkEdges (ID "hhh") [ID "ccc",ID "fff",ID "iii"],MkEdges (ID "iii") [Out]]

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
    numPathsFromNodeMemoised :: LazyMap.Map NodeID Int
    numPathsFromNodeMemoised
      = graph
        |> Map.toAscList
        |> LazyMap.fromAscList
        |> LazyMap.insert "out" [] -- "out" isn't in graph but is a valid input to our memoised function
        |> LazyMap.mapWithKey (\nodeID _ -> numPathsFromNode nodeID)

    numPathsFromNode :: NodeID -> Int
    numPathsFromNode nID
      | nID == "out" = 0 -- Base case: Got to "out" without finding targetID
      -- | nID `Set.member` seenNodes = 0 -- If nID has been seen before on this path then there's a cycle.
      --                                  -- If a path to our target contains a cycle, then there are inifinite paths.
      --                                  -- Since the 'inifinity' isn't a valid AoC answer, we assume that no paths to targetID contain a cycle,
      --                                  -- and therefore any path with a cycle will never lead to targetID.
      --                                  -- Therefore, we end the recursion and return 0.
      | targetID `elem` connectedNodes = 1 --[[nID,targetID]]
      | otherwise = sum (map (numPathsFromNodeMemoised !) connectedNodes)
      -- | otherwise = sum (map (numPathsFromNode (Set.insert nID seenNodes)) connectedNodes)
          -- = [ nID : path                  -- Add `nID` to the front of each recursive path
          --   | node <- connectedNodes      -- For each node in the connected nodes
          --   , path <- pathsFromNode node  -- Recursively find paths from that node
          --   ]
      where
        connectedNodes = graph ! nID
          -- = case graph Map.!? nID of
          --     Just ns -> ns
          --     _ -> error $ "NodeID (" ++ nID ++  ") was not in graph. Malformed input."

    m ! k = case m Map.!? k of
              Just ns -> ns
              _ -> error $ "NodeID (" ++ k ++  ") was not in graph. Malformed input."



detectCycle :: NodeID -> Map NodeID [NodeID] -> Either NodeID ()
detectCycle startNode graph = go Set.empty startNode
  where
    go seenNodes nID
      | nID == "out" = Right ()
      | nID `Set.member` seenNodes = Left nID
      | otherwise = traverse_ (go (Set.insert nID seenNodes)) connectedNodes
      where
        connectedNodes
          = case graph Map.!? nID of
              Just ns -> ns
              _ -> error $ "NodeID (" ++ nID ++  ") was not in graph. Malformed input."

dfsNodesFrom :: NodeID -> Map NodeID [NodeID] -> [NodeID]
dfsNodesFrom startNode graph = go Set.empty startNode
  where
    go seenNodes nID
      | nID == "out" = ["out"]
      | nID `Set.member` seenNodes = error "Cycle detected!"
      | otherwise = nID : concatMap (go (Set.insert nID seenNodes)) connectedNodes
      where
        connectedNodes
          = case graph Map.!? nID of
              Just ns -> ns
              _ -> error $ "NodeID (" ++ nID ++  ") was not in graph. Malformed input."


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
