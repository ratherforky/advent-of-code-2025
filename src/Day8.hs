{-# language MultilineStrings #-}
{-# language OverloadedRecordDot #-}
{-# language ImportQualifiedPost #-}
{-# language TypeApplications #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Day8 where

import AoCPrelude
import Data.Graph qualified as GraphStatic
import Data.Map qualified as Map
import Data.Graph.Inductive qualified as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Bits
import Data.Maybe

-------------
-- Parsing --
-------------

-- MultilineStrings extension, requires GHC 9.12.1 or above
egInput :: String
egInput = """
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"""

data Point3D = MkPoint
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Show, Eq, Ord)

pointP :: Parser Point3D
pointP = MkPoint <$> int <* char ',' <*> int <* char ',' <*> int

pointsP :: Parser [Point3D]
pointsP = everyLine pointP

-- >>> parseMaybe pointsP egInput
-- Just [Point 162 817 812,Point 57 618 57,Point 906 360 560,Point 592 479 940,Point 352 342 300,Point 466 668 158,Point 542 29 236,Point 431 825 988,Point 739 650 466,Point 52 470 668,Point 216 146 977,Point 819 987 18,Point 117 168 530,Point 805 96 715,Point 346 949 466,Point 970 615 88,Point 941 993 340,Point 862 61 35,Point 984 92 344,Point 425 690 689]

------------
-- Common --
------------

-- Uses OverloadedRecordDot to get fields of the Point3D record
dist :: Point3D -> Point3D -> Double
dist p q = sqrt (fromIntegral (square (q.x - p.x) + square (q.y - p.y) + square (q.z - p.z)))
  where
    square a = a ^ (2 :: Int)

------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 8 (task1 1000)

-- >>> task1 10 egInput
-- 40


task1 :: Int -> String -> Int
task1 numConnections
  = parseInput pointsP 
 .> (\ps -> [ (p,q) | p <- ps, q <- ps, p < q ]) -- All unique pairings of points.
                                                 -- `p < q` ensures no reflexive connections
                                                 -- and no duplicates of (p, q) and (q, p)
 .> sortOn (uncurry dist)                        -- Sort point pairings by ascending distance
 .> take numConnections                          -- Keep only the number of number of connections we're going to make
                                                 -- (10 for example, 1000 for full task)
 .> concatMap (\(p,q) -> [(p, [q]), (q, [p])])   -- Undirected graph, so include both directions
 .> Map.fromListWith (++)                        -- For each point `p`, merge connected edges using a Map
 .> Map.toList
 .> map (\(p,qs) -> (p, p, qs))                  -- Make `p` the node data and the key
 .> GraphStatic.graphFromEdges                   -- Build graph using Data.Graph
 .> (\(g,_,_) -> g)                              -- Ignore accessor functions and just keep Graph
 .> GraphStatic.scc                              -- Get strongly connected components
 .> map length                                   -- For each group of strongly connected components, get the size using length
 .> sortOn negate                                -- Sort sizes in descending order
 .> take 3                                       -- Take top 3 sizes
 .> product                                      -- Multiply


id :: a -> a
id x = x

------------
-- Task 2 --
------------

-- For task 2 we need to add edges dynamically, which Data.Graph can't do, so we use the `fgl` package instead

runTask2 :: IO Int
runTask2 = runDay 8 task2

-- >>> task2 egInput
-- 25272

task2 :: String -> Int
task2 str
  = points
    |> (\ps -> [ (p,q) | p <- ps, q <- ps, p < q ])
    |> sortOn (uncurry dist)

    -- lazy scanl adding one edge at a time and tracking the last edge added
    |> scanl step (initGraph, (MkPoint 0 0 0, MkPoint 0 0 0))

    -- lazy find which will go through the list from scanl and 
    -- exit early once a fullyConnected graph is found,
    -- so the scanl won't keep adding edges past the point of the graph being fully connected
    |> find (fst .> fullyConnected)
    |> fromJust
    |> (\(_, (p,q)) -> p.x * q.x)
  where
    points = parseInput pointsP str
    nodes = map (\p -> (hashPoint p, ())) points
    initGraph = FGL.mkGraph @Gr nodes [] -- Using TypeApplications extension to specify which graph type to use

    step (g, _) (p,q) = ( FGL.insEdge (hashPoint p, hashPoint q, ()) g
                        , (p,q)
                        )

    -- When undirected depth-first search from an arbitrary node produces all of nodes
    -- we know the graph is fully connected. Fails quickly early on when node isn't well connected.
    fullyConnected g = length (FGL.udfs [fst (head nodes)] g) == numNodes
    numNodes = length nodes


-- Works as long as `Int` is 64 bit and all coordinates are less than 2^20
hashPoint :: Point3D -> Int
hashPoint p = p.x + (p.y .<<. 20) + (p.z .<<. 40) -- Bit shifting

-- >>> showBitsBigEndian 0 64 (hashPoint (MkPoint 40878 89496 50699))
-- "0000000011000110000010110001010111011001100000001001111110101110"

inverseHashPoint :: Int -> Point3D
inverseHashPoint v = MkPoint (fromBits 0 20 v) (fromBits 20 20 v) (fromBits 40 20 v)

-- >>> inverseHashPoint (hashPoint (MkPoint 40878 89496 50699))
-- MkPoint {x = 40878, y = 89496, z = 50699}

fromBits :: Int -> Int -> Int -> Int
fromBits firstBit numBits v = (v .>>. firstBit) .&. (2 ^ numBits - 1)

-- >>> fromBits 0 20 24124
-- 24124

showBitsBigEndian :: Bits a => Int -> Int -> a -> String
showBitsBigEndian firstBit numBits v
  = reverse
      [ if testBit v j then '1' else '0'
      | j <- [firstBit .. firstBit + numBits - 1] 
      ]
