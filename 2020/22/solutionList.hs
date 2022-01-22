import Data.List
import qualified Data.Set as S
import Debug.Trace (trace)
import Prelude hiding (either)

main :: IO ()
main = do
  -- Sample
  -- let player1 = [9, 2, 6, 3, 1]
  -- let player2 = [5, 8, 4, 7, 10]

  -- Infinite
  -- let player1 = [43, 19]
  -- let player2 = [2, 29, 14]

  -- Input
  let player1 = [47, 19, 22, 31, 24, 6, 10, 5, 1, 48, 46, 27, 8, 45, 16, 28, 33, 41, 42, 36, 50, 39, 30, 11, 17]
  let player2 = [4, 18, 21, 37, 34, 15, 35, 38, 20, 23, 9, 25, 32, 13, 26, 2, 12, 44, 14, 49, 3, 40, 7, 43, 29]

  print "Start 1"
  print $ eval1 $ play1 player1 player2
  print "Done with 1"
  let (score1, score2) = play2 0 S.empty player1 player2
  print ["Score player 1", show $ eval1 score1]
  print ["Score player 2", show $ eval1 score2]

eval1 :: [Int] -> Int
eval1 xs = sum $ zipWith (*) [1 ..] $ reverse xs

play1 :: [Int] -> [Int] -> [Int]
play1 [] ys = ys
play1 xs [] = xs
play1 (x : xs) (y : ys)
  | x > y = play1 (xs ++ [x, y]) ys
  | otherwise = play1 xs (ys ++ [y, x])

type Deck = [Int]

play2 :: Int -> S.Set (Deck, Deck) -> Deck -> Deck -> (Deck, Deck)
-- play2 s prev xss yss = trace (show [show xss, show yss]) $ play2' s prev xss yss
play2 = play2'

play2' :: Int -> S.Set (Deck, Deck) -> Deck -> Deck -> (Deck, Deck)
play2' _ prev [] ys = ([], ys)
play2' _ prev xs [] = (xs, [])
play2' subgameLevel prev xss@(x : xs) yss@(y : ys)
  | (xss, yss) `S.member` prev = (xss, [])
  | (x > length xs || y > length ys) && x < y = play2 subgameLevel (S.insert (xss, yss) prev) xs (ys ++ [y, x])
  | (x > length xs || y > length ys) && x > y = play2 subgameLevel (S.insert (xss, yss) prev) (xs ++ [x, y]) ys
  | otherwise =
    let newXs = take x xs
        newYs = take y ys
        -- (resX, resY) = trace (show ["starting level ", show (subgameLevel + 1)]) play2 (subgameLevel + 1) S.empty newXs newYs
        (resX, resY) = play2 (subgameLevel + 1) S.empty newXs newYs
        xWins = not $ null resX
     in if xWins
          then play2 subgameLevel (S.insert (xss, yss) prev) (xs ++ [x, y]) ys
          else play2 subgameLevel (S.insert (xss, yss) prev) xs (ys ++ [y, x])
