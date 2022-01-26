import Data.IntMap (IntMap)
import Data.IntMap.Strict (insert, size, (!))
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import qualified Data.List as List
import Prelude hiding (filter, reverse, sort, splitAt, take)

-- a map that models a singly linked list e.g. (1,2) means 2 is to the right of 1.
-- Insertion is just an update of that map
type Ring = IntMap Int

type RingSegment = IntMap Int

data GameState = GameState {gStart :: Int, gRing :: Ring}

instance Show GameState where
  show (GameState x y) = show (x, pp y)

example = fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]

input1 = fromList [9, 5, 2, 3, 1, 6, 4, 8, 7]

main :: IO ()
main = do
  print "start"
  let folution1 = iterate play input1
  print $ folution1 !! 100

pp :: Ring -> [Int]
pp ring = 1 : go (ring ! 1)
  where
    go 1 = []
    go n = n : go (ring ! n)

fromList :: [Int] -> GameState
fromList l = GameState (head l) $ M.fromList $ zip l (drop 1 l ++ [head l])

-- The crab picks up the three cups that are immediately clockwise of the current cup.
-- They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
-- The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up. If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.
-- The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. They keep the same order as when they were picked up.
-- The crab selects a new current cup: the cup which is immediately clockwise of the current cup.

toIndex :: Int -> Int -> Int
toIndex s i =
  let m = i `mod` s
   in m `seq` if m == 0 then s else m

getDestination currentCup selected ring =
  let pot = map (toIndex (size ring)) [currentCup - 1, currentCup -2, currentCup -3, currentCup -4]
      res = head $ List.filter (`List.notElem` selected) pot
   in res

--    [(currentCup, a), (a, b), (b, c), (c, nextCup), (nextCup, foo), .., (destination, x), (x, y)]
-- -> [(currentCup, nextCup), (nextCup, foo), .., (destination, a), (a, b), (b, c), (c, x) (x, y)]
-- need: nextCup, a, c, destination, x
play :: GameState -> GameState
play (GameState currentCup ring) =
  let a = ring ! currentCup
      b = ring ! a
      c = ring ! b
      nextCup = ring ! c
      destination = getDestination currentCup [a, b, c] ring
      x = ring ! destination
      toInsert = [(currentCup, nextCup), (destination, a), (c, x)]
      newRing = foldl' (\y (k, v) -> insert k v y) ring toInsert
   in GameState nextCup newRing

--  in trace (show [show toInsert, show $ pp ring, show $ pp newRing]) $ GameState nextCup newRing
