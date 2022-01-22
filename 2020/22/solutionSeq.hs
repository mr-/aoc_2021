import Data.List hiding (length, null, reverse, take, zipWith)
import Data.Sequence
import qualified Data.Set as Set
import Debug.Trace (trace)
import Prelude hiding (either, length, null, reverse, take, zipWith)

main :: IO ()
main = do
  -- Sample
  -- let player1 = fromList [9, 2, 6, 3, 1]
  -- let player2 = fromList [5, 8, 4, 7, 10]

  -- Infinite
  -- let player1 = fromList [43, 19]
  -- let player2 = fromList [2, 29, 14]

  -- Input
  let player1 = fromList [47, 19, 22, 31, 24, 6, 10, 5, 1, 48, 46, 27, 8, 45, 16, 28, 33, 41, 42, 36, 50, 39, 30, 11, 17]
  let player2 = fromList [4, 18, 21, 37, 34, 15, 35, 38, 20, 23, 9, 25, 32, 13, 26, 2, 12, 44, 14, 49, 3, 40, 7, 43, 29]

  print "Start 1"
  print $ eval1 $ play1 player1 player2
  print "Done with 1"
  let (score1, score2) = play2 0 Set.empty player1 player2
  print ["Score player 1", show $ eval1 score1]
  print ["Score player 2", show $ eval1 score2]

eval1 :: Seq Int -> Int
eval1 xs = sum $ zipWith (*) (fromList [1 .. length xs]) $ reverse xs

play1 :: Seq Int -> Seq Int -> Seq Int
play1 Empty ys = ys
play1 xs Empty = xs
play1 (x :<| xs) (y :<| ys)
  | x > y = play1 (xs :|> x :|> y) ys
  | otherwise = play1 xs (ys :|> y :|> x)

type Deck = Seq Int

play2 :: Int -> Set.Set (Deck, Deck) -> Deck -> Deck -> (Deck, Deck)
-- play2 s prev xss yss = trace (show [show xss, show yss]) $ play2' s prev xss yss
play2 = play2'

play2' :: Int -> Set.Set (Deck, Deck) -> Deck -> Deck -> (Deck, Deck)
play2' _ prev Empty ys = (Empty, ys)
play2' _ prev xs Empty = (xs, Empty)
play2' subgameLevel prev xss@(x :<| xs) yss@(y :<| ys)
  | (xss, yss) `Set.member` prev = (xss, Empty)
  | (x > length xs || y > length ys) && x < y = play2 subgameLevel (Set.insert (xss, yss) prev) xs (ys :|> y :|> x)
  | (x > length xs || y > length ys) && x > y = play2 subgameLevel (Set.insert (xss, yss) prev) (xs :|> x :|> y) ys
  | otherwise =
    let newXs = take x xs
        newYs = take y ys
        -- (resX, resY) = trace (show ["starting level ", show (subgameLevel + 1)]) play2 (subgameLevel + 1) S.empty newXs newYs
        (resX, resY) = play2 (subgameLevel + 1) Set.empty newXs newYs
        xWins = not $ null resX
     in if xWins
          then play2 subgameLevel (Set.insert (xss, yss) prev) (xs :|> x :|> y) ys
          else play2 subgameLevel (Set.insert (xss, yss) prev) xs (ys :|> y :|> x)
