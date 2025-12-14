-- {-# language QuasiQuotes #-}
{-# language OverloadedRecordDot #-}
{-# language ImportQualifiedPost #-}
{-# language TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Day10 where

import AoCPrelude

import Control.Monad

import Data.Graph.Inductive (LNode, LEdge)
import Data.Graph.Inductive qualified as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Dot
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import System.Process

-- import Math.Tensor.LinearAlgebra.Matrix
import Math.LinearEquationSolver
import Language.Hasmtlib as SMT
import Language.Hasmtlib.Type.Solver (solveMinimized) -- Doesn't compile with GHC 9.12 https://github.com/bruderj15/Hasmtlib/issues/125

import Debug.Todo
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)
import Data.Maybe (fromJust)

-------------
-- Parsing --
-------------

egInput :: String
egInput = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"


data Machine = MkInputLine
  { target   :: [Light]
  , buttons  :: [Button]
  , joltages :: [Integer]
  }
  deriving (Show, Eq)
data Light = Off | On deriving (Show, Eq, Ord, Enum)
type Button = [LightID]
type LightID = Int

switch :: Light -> Light
switch On  = Off
switch Off = On

lightP :: Parser Light
lightP = On <$ char '#'
     <|> Off <$ char '.'

buttonP :: Parser Button
buttonP = delim' (tok "(") int (char ',') (tok ")")

joltagesP :: Parser [Integer]
joltagesP = delim' (tok "{") (fromIntegral <$> int) (char ',') (tok "}")

machineP :: Parser Machine
machineP
  = MkInputLine 
      <$  char '[' <*> some lightP <* tok "]"
      <*> (some buttonP)
      <*> joltagesP

machinesP :: Parser [Machine]
machinesP = everyLine machineP

-- >>> parseMaybe machinesP egInput
-- Just [MkInputLine {lights = [Off,On,On,Off], buttons = [[3],[1,3],[2],[2,3],[0,2],[0,1]], joltages = [3,5,4,7]},MkInputLine {lights = [Off,Off,Off,On,Off], buttons = [[0,2,3,4],[2,3],[0,4],[0,1,2],[1,2,3,4]], joltages = [7,5,12,7,2]},MkInputLine {lights = [Off,On,On,On,Off,On], buttons = [[0,1,2,3,4],[0,3,4],[0,1,2,4,5],[1,2]], joltages = [10,11,11,5,10,5]}]

egMachines :: [Machine]
egMachines = parseInput machinesP egInput

-- >>> egMachines
-- [MkInputLine {lights = [Off,On,On,Off], buttons = [[3],[1,3],[2],[2,3],[0,2],[0,1]], joltages = [3,5,4,7]},MkInputLine {lights = [Off,Off,Off,On,Off], buttons = [[0,2,3,4],[2,3],[0,4],[0,1,2],[1,2,3,4]], joltages = [7,5,12,7,2]},MkInputLine {lights = [Off,On,On,On,Off,On], buttons = [[0,1,2,3,4],[0,3,4],[0,1,2,4,5],[1,2]], joltages = [10,11,11,5,10,5]}]

egMachine :: Machine
egMachine = head egMachines

-- >>> egMachine
-- MkInputLine {lights = [Off,On,On,Off], buttons = [[3],[1,3],[2],[2,3],[0,2],[0,1]], joltages = [3,5,4,7]}

egTarget :: [Light]
egTarget = egMachine.target

-- >>> egTarget
-- [Off,On,On,Off]

egButtons :: [Button]
egButtons = egMachine.buttons


------------
-- Task 1 --
------------

runTask1 :: IO Int
runTask1 = runDay 10 task1

-- >>> task1 egInput
-- [2,3,2]


task1 :: String -> Int
task1
  = parseInput machinesP
 .> map minButtons
 .> sum


makeEgGraphPNG :: IO ()
makeEgGraphPNG = do
  writeFile "egGraph.dot" (showDot (fglToDot egGraph))
  callCommand "dot -T png -o egGraph.png egGraph.dot"

-- >>> FGL.lesp (egBimap Bimap.!> [Off,Off,Off,Off]) (egBimap Bimap.!> [Off,On,On,Off]) egGraph
-- [(0,[3]),(3,[2,3]),(6,[1,3])]
-- First edge label meaningless.

minButtons :: Machine -> Int
minButtons m
  = FGL.lesp node node' graph
    |> FGL.unLPath
    |> drop 1
    |> length
  where
    (graph, nodeLightsBimap) = makeMachineGraph m
    node  = nodeLightsBimap Bimap.!> (Off <$ m.target)
    node' = nodeLightsBimap Bimap.!> m.target

egGraph :: Gr [Light] Button
egGraph = fst (makeMachineGraph egMachine)

makeMachineGraph :: Machine -> (Gr [Light] Button, Bimap FGL.Node [Light])
makeMachineGraph m = (FGL.mkGraph nodes edges, nodeBimap)
  where
    lightStates = allLightStates m.target

    nodes :: [LNode [Light]]
    nodes = zipWith (,) [0..] lightStates

    nodeBimap = Bimap.fromList nodes

    edges :: [LEdge Button]
    edges = concatMap (makeEdges m.buttons nodeBimap) lightStates

-- >>> makeMachineGraph egMachine
-- mkGraph [(0,[Off,Off,Off,Off]),(1,[Off,Off,Off,On]),(2,[Off,Off,On,Off]),(3,[Off,Off,On,On]),(4,[Off,On,Off,Off]),(5,[Off,On,Off,On]),(6,[Off,On,On,Off]),(7,[Off,On,On,On]),(8,[On,Off,Off,Off]),(9,[On,Off,Off,On]),(10,[On,Off,On,Off]),(11,[On,Off,On,On]),(12,[On,On,Off,Off]),(13,[On,On,Off,On]),(14,[On,On,On,Off]),(15,[On,On,On,On])] [(0,1,[3]),(0,2,[2]),(0,3,[2,3]),(0,5,[1,3]),(0,10,[0,2]),(0,12,[0,1]),(1,0,[3]),(1,2,[2,3]),(1,3,[2]),(1,4,[1,3]),(1,11,[0,2]),(1,13,[0,1]),(2,0,[2]),(2,1,[2,3]),(2,3,[3]),(2,7,[1,3]),(2,8,[0,2]),(2,14,[0,1]),(3,0,[2,3]),(3,1,[2]),(3,2,[3]),(3,6,[1,3]),(3,9,[0,2]),(3,15,[0,1]),(4,1,[1,3]),(4,5,[3]),(4,6,[2]),(4,7,[2,3]),(4,8,[0,1]),(4,14,[0,2]),(5,0,[1,3]),(5,4,[3]),(5,6,[2,3]),(5,7,[2]),(5,9,[0,1]),(5,15,[0,2]),(6,3,[1,3]),(6,4,[2]),(6,5,[2,3]),(6,7,[3]),(6,10,[0,1]),(6,12,[0,2]),(7,2,[1,3]),(7,4,[2,3]),(7,5,[2]),(7,6,[3]),(7,11,[0,1]),(7,13,[0,2]),(8,2,[0,2]),(8,4,[0,1]),(8,9,[3]),(8,10,[2]),(8,11,[2,3]),(8,13,[1,3]),(9,3,[0,2]),(9,5,[0,1]),(9,8,[3]),(9,10,[2,3]),(9,11,[2]),(9,12,[1,3]),(10,0,[0,2]),(10,6,[0,1]),(10,8,[2]),(10,9,[2,3]),(10,11,[3]),(10,15,[1,3]),(11,1,[0,2]),(11,7,[0,1]),(11,8,[2,3]),(11,9,[2]),(11,10,[3]),(11,14,[1,3]),(12,0,[0,1]),(12,6,[0,2]),(12,9,[1,3]),(12,13,[3]),(12,14,[2]),(12,15,[2,3]),(13,1,[0,1]),(13,7,[0,2]),(13,8,[1,3]),(13,12,[3]),(13,14,[2,3]),(13,15,[2]),(14,2,[0,1]),(14,4,[0,2]),(14,11,[1,3]),(14,12,[2]),(14,13,[2,3]),(14,15,[3]),(15,3,[0,1]),(15,5,[0,2]),(15,10,[1,3]),(15,12,[2,3]),(15,13,[2]),(15,14,[3])]

allLightStates :: [Light] -> [[Light]]
allLightStates
  = foldr (\_ acc -> [ light : lightStates  -- Generate all possible light states for the number of lights available
                     | light <- [Off, On]
                     , lightStates <- acc
                     ])
          [[]]

task1Nodes :: [Light] -> [LNode [Light]]
task1Nodes
  = allLightStates
 .> zipWith (,) [0..] -- Give them each an ID and make them a node labeled with the state

-- >>> task1Nodes egTarget
-- [(0,[Off,Off,Off,Off]),(1,[Off,Off,Off,On]),(2,[Off,Off,On,Off]),(3,[Off,Off,On,On]),(4,[Off,On,Off,Off]),(5,[Off,On,Off,On]),(6,[Off,On,On,Off]),(7,[Off,On,On,On]),(8,[On,Off,Off,Off]),(9,[On,Off,Off,On]),(10,[On,Off,On,Off]),(11,[On,Off,On,On]),(12,[On,On,Off,Off]),(13,[On,On,Off,On]),(14,[On,On,On,Off]),(15,[On,On,On,On])]

egBimap :: Bimap FGL.Node [Light]
egBimap = Bimap.fromList (task1Nodes egTarget)

makeEdges :: [Button] -> Bimap FGL.Node [Light] -> [Light] -> [LEdge Button]
makeEdges bs nodeLightsMap lightsState = do
  b <- bs
  let lightsState' = pressButton b lightsState
      node  = nodeLightsMap Bimap.!> lightsState
      node' = nodeLightsMap Bimap.!> lightsState'
  pure (node, node', b)

-- >>> egBimap
-- fromList [(0,[Off,Off,Off,Off]),(1,[Off,Off,Off,On]),(2,[Off,Off,On,Off]),(3,[Off,Off,On,On]),(4,[Off,On,Off,Off]),(5,[Off,On,Off,On]),(6,[Off,On,On,Off]),(7,[Off,On,On,On]),(8,[On,Off,Off,Off]),(9,[On,Off,Off,On]),(10,[On,Off,On,Off]),(11,[On,Off,On,On]),(12,[On,On,Off,Off]),(13,[On,On,Off,On]),(14,[On,On,On,Off]),(15,[On,On,On,On])]

-- >>> makeEdges egButtons egBimap [Off,Off,Off,Off]
-- [(0,1,[3]),(0,5,[1,3]),(0,2,[2]),(0,3,[2,3]),(0,10,[0,2]),(0,12,[0,1])]

pressButton :: Button -> [Light] -> [Light]
pressButton button
  = foldr (\lightID update -> toggleAtIndex lightID .> update) id button

toggleAtIndex :: LightID -> [Light] -> [Light]
toggleAtIndex i = go 0
  where
    go j (l:ls)
      | j == i = switch l : ls
      | j <  i = l : go (j + 1) ls
      | otherwise = error "Index out of bounds"
    go _ _ = error "Index out of bounds (got to empty list)"

-- >>> toggleAtIndex 2 [Off,Off,Off,Off]
-- [Off,Off,On,Off]

------------
-- Task 2 --
------------

runTask2 :: IO Integer
runTask2 = readFile "src/inputs/Day10Input.txt" >>= task2

-- >>> runTask2

-- >>> task2 egInput
-- 33

task2 :: String -> IO Integer
task2
  = parseInput machinesP
 .> traverse solveMinButtonPresses
 .> fmap (map (snd .> fromJust) .> sum)

solveMinButtonPresses :: Machine -> IO (Result, Maybe Integer)
solveMinButtonPresses m = solveWith @OMT (solver z3) $ do

  -- Make variables for each button.
  -- These will be the number of times they need to be pressed.
  -- We will use their list index to refer to them
  vs <- sequence ((var @IntSort) <$ m.buttons)

  -- All variables are the number of button presses for a particular button,
  -- therefore they must always be 0 or more. No negative presses.
  for_ vs $ \v -> do
    assert $ v >=? 0            


  for_ (zip [0 ..] m.joltages) $ \(light, targetJoltage) -> do
    let buttonVarsWhichSwitchLight
          = zip [0 :: Int ..] m.buttons
            |> filter (\(_,bs) -> light `elem` bs)
            |> map (fst .> (vs !!))

    assert $ sum buttonVarsWhichSwitchLight
                === fromInteger targetJoltage

  let totalButtonPresses = sum vs

  minimize totalButtonPresses

  pure totalButtonPresses


-- testHasmtlib :: IO (Result, Maybe Integer)
testHasmtlib :: IO (Result, Maybe [Integer])
testHasmtlib = solveWith @OMT (solver z3) $ do
-- testHasmtlib = solveWith @OMT (solver $ debugging verbosely z3) $ do
    setLogic "QF_LIA" -- "QF_LIA"

    [a,b,c,d,e,f] <- sequence (replicate 6 (var @IntSort))

    assert $ e + f === 3
    assert $ b + f === 5
    assert $ c + d + e === 4
    assert $ a + b + d === 7
    -- assert $ x `SMT.mod` 42 === y
    -- assert $ y + x + 1 >=? x + y
    -- assert $ (a + b + c + d + e + f) <? 11
    traverse_ (assert . (>=? 0)) [a,b,c,d,e,f]

    minimize (sum [a,b,c,d,e,f])

    pure [a,b,c,d,e,f]


    -- setLogic "QF_LIRA"

    -- xs <- replicateM 10 $ var @RealSort

    -- forM_ xs $ \x -> assert $ x >=? 0 SMT.&& x <? fromIntegral (length xs)
    -- forM_ xs $ assert . isIntSort
    -- assert $ distinct xs

    -- return xs
  
-- testHasmtlib :: IO  (Result,  Maybe (Decoded (Result, Solution)))
  -- solveWith @Pipe (solver z3) $ do
  --   setLogic "QF_LIA"

  --   [a,b,c,d,e,f] <- sequence (replicate 6 (var @IntSort))

  --   assert $ e + f === 3
  --   assert $ b + f === 5
  --   assert $ c + d + e === 4
  --   assert $ a + b + d === 7
  --   -- assert $ x `SMT.mod` 42 === y
  --   -- assert $ y + x + 1 >=? x + y

  --   let total = sum [a,b,c,d,e,f]

  --   -- return [a,b,c,d,e,f]
  --   solveMinimized total Nothing Nothing

-- testSolver :: IO (Maybe [Integer])
-- testSolver = solveIntegerLinearEqs Z3 [[2, 3, 4],[6, -3, 9],[2, 0, 1]] [20, -6, 8]

-- testSolverMachine1 :: IO [[Integer]]
-- testSolverMachine1 
--   = solveIntegerLinearEqsAll Z3 1000
--       [ [0,0,0,0,1,1]
--       , [0,1,0,0,0,1]
--       , [0,0,1,1,1,0]
--       , [1,1,0,1,0,0]
--       ]
--       [3,5,4,7]
