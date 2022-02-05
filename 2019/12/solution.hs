module Main where

import qualified Data.HashSet as HashSet
import Data.VectorSpace ((^+^))
import Debug.Trace (trace)

data Planet = Planet {position :: Pos, velocity :: Velocity} deriving (Show, Ord, Eq)

type Pos = (Int, Int, Int)

type Velocity = (Int, Int, Int)

main = do
  print "Solution 1"
  print solution1
  print "Solution 2"
  print $ sol2 puzzleInput

solution1 = energyAfter puzzleInput 1000

pl p = Planet p (0, 0, 0)

-- <x=-1, y=0, z=2>
-- <x=2, y=-10, z=-7>
-- <x=4, y=-8, z=8>
-- <x=3, y=5, z=-1>
examplePlanets1 :: [Planet]
examplePlanets1 = [pl (-1, 0, 2), pl (2, -10, -7), pl (4, -8, 8), pl (3, 5, -1)]

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>
examplePlanets2 = [pl (-8, -10, 0), pl (5, 5, 10), pl (2, -7, 3), pl (9, -8, -3)]

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>
examplePlanets3 = [pl (-8, -10, 0), pl (5, 5, 10), pl (2, -7, 3), pl (9, -8, -3)]

-- <x=-9, y=-1, z=-1>
-- <x=2, y=9, z=5>
-- <x=10, y=18, z=-12>
-- <x=-6, y=15, z=-7>
puzzleInput = [pl (-9, -1, -1), pl (2, 9, 5), pl (10, 18, -12), pl (-6, 15, -7)]

energyAfter ps n = totalEnergy $ simulation ps !! n

energy :: Planet -> Int
energy (Planet p v) = e p * e v
  where
    e (x, y, z) = abs x + abs y + abs z

totalEnergy = sum . map energy

simulation :: [Planet] -> [[Planet]]
simulation = iterate doStep

-- To apply gravity, consider every pair of moons. On each axis (x, y, and z),
-- the velocity of each moon changes by exactly +1 or -1 to pull the moons together.
-- For example, if Ganymede has an x position of 3, and Callisto has a x position of 5,
-- then Ganymede's x velocity changes by +1 (because 5 > 3) and Callisto's x velocity changes by -1 (because 3 < 5).
-- However, if the positions on a given axis are the same, the velocity on that axis does not change for that pair of moons.
doStep :: [Planet] -> [Planet]
doStep ps = map (\p -> getTotalGravity p ps) ps

getTotalGravity :: Planet -> [Planet] -> Planet
getTotalGravity p@(Planet pos velo) ps =
  let newVelocity = foldr (^+^) velo $ map (getNewVelocity p) ps
   in Planet (pos ^+^ newVelocity) newVelocity

getNewVelocity :: Planet -> Planet -> Velocity
getNewVelocity (Planet (x, y, z) _) (Planet (x', y', z') _) = (d x x', d y y', d z z')
  where
    d :: Int -> Int -> Int
    d u v
      | u > v = -1
      | u < v = 1
      | otherwise = 0



-- Oh well.. the "stupid" approach is too slow. But we can model the coordinates independently of each other..
-- So we determine all "cycle counts" individually and take the lcm of those 3.
type SliceVelo = Int

type Slice = (Int, Int) -- pos and velo

type Slices = [(Int, Int)]

repeats :: Int -> [Slices] -> HashSet.HashSet Slices -> Int
repeats n [] _ = undefined
repeats n (s : ss) seen
  | s `HashSet.member` seen = n
  | otherwise = repeats (n + 1) ss (s `HashSet.insert` seen)

iteratedSlice :: ((Int, Int, Int) -> Int) -> [Planet] -> [Slices]
iteratedSlice pr input = iterate doSliceStep (toSlice pr input)

sol2 :: [Planet] -> Int
sol2 input =
  let xs = iteratedSlice (\(a, b, c) -> a) input
      ys = iteratedSlice (\(a, b, c) -> b) input
      zs = iteratedSlice (\(a, b, c) -> c) input
      rep x = repeats 0 x HashSet.empty
      (x, y, z) = (rep xs, rep ys, rep zs)
   in lcm x (lcm y z)

toSlice :: ((Int, Int, Int) -> Int) -> [Planet] -> [Slice]
toSlice pos ps = map (\(Planet p v) -> (pos p, pos v)) ps

doSliceStep :: Slices -> Slices
doSliceStep ps = map (\p -> getTotalSliceGravity p ps) ps

getTotalSliceGravity :: Slice -> Slices -> Slice
getTotalSliceGravity s ss =
  let newVelo = snd s + sum (map (getNewSliceVelocity s) ss)
   in (fst s + newVelo, newVelo)

getNewSliceVelocity :: Slice -> Slice -> SliceVelo
getNewSliceVelocity s t = d (fst s) (fst t)
  where
    d :: Int -> Int -> Int
    d u v
      | u > v = -1
      | u < v = 1
      | otherwise = 0
