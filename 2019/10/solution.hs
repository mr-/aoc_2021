import Control.Monad (guard)
import Data.List (group, groupBy, sort, sortBy, sortOn)
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.Real (Ratio)

type Point = (Integer, Integer)

type Slope = (Ratio Integer, Ratio Integer)

angle :: Floating a => Slope -> a
angle p = - (angle' p - 0.5)

angle' :: Floating a => Slope -> a
angle' (0, b) = if b > 0 then 1 / 2 else - 1 / 2
angle' (a, b) =
  let at = atan (fromRational b / fromRational a) / pi
      r = if a < 0 then at - 1 else at
   in r

cmp :: Slope -> Slope -> Ordering
cmp p q = angle p `compare` angle q

slopes :: [Slope]
slopes = [(0, 1), (1, 2), (1, 1), (2, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

test = map angle slopes

main :: IO ()
main = do
  print "Solution 1"
  content <- readFile "input.txt"
  let asteroids = Set.fromList $ toCoords (lines content)
  let (vMax, vMaxPoint) = findMax asteroids
  print (vMax, vMaxPoint)

  print "Solution 2"
  let vs = sortSlopes $ visibles vMaxPoint asteroids
  let (a, b) = last $ shoot vs
  print $ a * 100 + b

shoot :: [(Slope, [Point])] -> [Point]
shoot = shoot' 1
  where
    shoot' 200 l = [head $ snd $ head l]
    shoot' n ((s, [p]) : xs) = p : shoot' (n + 1) xs
    shoot' n ((s, p : ps) : xs) = p : shoot' (n + 1) (xs ++ [(s, ps)])
    shoot' n xs = error $ show ("WTF", n, length xs)

findMax :: Set.Set Point -> (Int, Point)
findMax asteroids =
  let coords = Set.toList asteroids
      vis = map (\x -> (length $ visibles x asteroids, x)) coords
   in maximum vis

sortSlopes :: [(Slope, [Point])] -> [(Slope, [Point])]
sortSlopes = sortBy (\p q -> cmp (fst p) (fst q))

visibles :: Point -> Set.Set Point -> [(Slope, [Point])]
visibles (x, y) asteroids =
  let norm (a, b) = maximum [abs a, abs b]
      normed p@(a, b) = (a % norm p, b % norm p)
      toSlope (a, b) = normed (a - x, y - b)
      slopes = map (\p -> (toSlope p, p)) (Set.toList $ Set.delete (x, y) asteroids)
      sortedSlopes = sortBy (comparing fst) slopes
      groupedBySlopes = groupBy (\p q -> fst p == fst q) sortedSlopes
      accumulated = map (\l -> (fst $ head l, sortOn (\(a, b) -> norm (a - x, b - y)) $ map snd l)) groupedBySlopes
   in accumulated

toCoords :: [String] -> [Point]
toCoords lines = do
  line <- zip [0 ..] lines
  char <- zip [0 ..] (snd line)
  guard $ snd char == '#'

  return (fst char, fst line)
