import Control.Monad (guard)
import qualified Data.Set as Set

type Point = (Int, Int, Int)

type Board = Set.Set Point

main :: IO ()
main = do
  print "start"
  content <- readFile "input.txt"
  let board = Set.fromList $ toCoords (lines content)
  let resultingBoard = iterate doStep board !! 6

  print $ Set.size resultingBoard

toCoords :: [[Char]] -> [Point]
toCoords lines = do
  line <- zip [0 ..] lines
  char <- zip [0 ..] (snd line)
  guard $ snd char == '#'

  return (fst char, fst line, 0)

doStep :: Board -> Board
doStep board =
  let uniquePoints = Set.fromList $ pointsToCheck board
      newPoints = Set.filter (lives board) uniquePoints
   in newPoints

pointsToCheck :: Board -> [Point]
pointsToCheck board = do
  point <- Set.elems board
  point : getCandidates point

getCandidates :: Point -> [Point]
getCandidates (x, y, z) = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  dz <- [-1, 0, 1]
  guard $ not $ dx == 0 && dy == 0 && dz == 0

  return (x + dx, y + dy, z + dz)

criteria :: Bool -> Int -> Bool
-- If a cube is active and exactly 2 or 3 of its neighbors are also active,
-- the cube remains active. Otherwise, the cube becomes inactive.
criteria True 2 = True
criteria True 3 = True
criteria True _ = False
-- If a cube is inactive but exactly 3 of its neighbors are active,
-- the cube becomes active. Otherwise, the cube remains inactive.
criteria False 3 = True
criteria False _ = False

lives :: Board -> Point -> Bool
lives board point =
  let cands = Set.fromList $ getCandidates point
      neighbours = Set.size $ Set.intersection board cands
      self = Set.member point board
   in criteria self neighbours
