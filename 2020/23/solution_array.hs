import Control.Monad (forM)
import Control.Monad.ST
import Data.Foldable (Foldable (toList), forM_)
import Data.List (foldl')
import qualified Data.List as List
import qualified Data.Vector.Unboxed.Mutable as M
import Prelude

-- an array that models a singly linked list e.g. (1,2) means 2 is to the right of 1.
-- Insertion is just an update of that array
type Ring s = ST s (M.MVector s Int)

data GameState s = GameState {gStart :: Int, gRing :: Ring s}

main :: IO ()
main = do
  print "start"
  print $ playAll puzzleInput 9

fromList :: [Int] -> ST s (M.MVector s Int)
fromList l = do
  arr <- M.replicate (1 + length l) 0
  forM (zip l (drop 1 l ++ [head l])) (\(x, y) -> M.write arr x y)

  return arr

example = fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]

example2 = [3, 8, 9, 1, 2, 5, 4, 6, 7] ++ [10 .. 1000000]

input1 = fromList [9, 5, 2, 3, 1, 6, 4, 8, 7]

puzzleInput = [9, 5, 2, 3, 1, 6, 4, 8, 7] ++ [10 .. 1000000]

-- The crab picks up the three cups that are immediately clockwise of the current cup.
-- They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
-- The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up. If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.
-- The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. They keep the same order as when they were picked up.
-- The crab selects a new current cup: the cup which is immediately clockwise of the current cup.

toIndex :: Int -> Int -> Int
toIndex s i =
  let m = i `mod` s
   in m `seq` if m == 0 then s else m

getDestination :: (M.Unbox a, Foldable t) => Int -> t Int -> M.MVector s a -> Int
getDestination currentCup selected ring =
  let pot = map (toIndex (M.length ring - 1)) [currentCup - 1, currentCup -2, currentCup -3, currentCup -4]
      res = head $ List.filter (`List.notElem` selected) pot
   in res

--    [(currentCup, a), (a, b), (b, c), (c, nextCup), (nextCup, foo), .., (destination, x), (x, y)]
-- -> [(currentCup, nextCup), (nextCup, foo), .., (destination, a), (a, b), (b, c), (c, x) (x, y)]
-- need: nextCup, a, c, destination, x
play :: (Int, M.MVector s Int) -> ST s (Int, M.MVector s Int)
play (currentCup, ring) = do
  a <- M.read ring currentCup
  b <- M.read ring a
  c <- M.read ring b
  nextCup <- M.read ring c
  let destination = getDestination currentCup [a, b, c] ring
  x <- M.read ring destination
  let toInsert = [(currentCup, nextCup), (destination, a), (c, x)]
  forM_ toInsert (\(k, v) -> M.write ring k v)
  return (nextCup, ring)

repeatM n f = foldr (\f g x -> f x >>= g) return (replicate n f)

playAll :: [Int] -> Int -> Int
playAll l a = runST $ do
  arr <- fromList l
  foo <- repeatM 10000000 play (a, arr)
  x <- M.read arr 1
  y <- M.read arr x
  return $ x * y
