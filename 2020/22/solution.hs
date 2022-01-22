import Data.List
import Prelude hiding (either)

main :: IO ()
main = do
  -- let player1 = [9, 2, 6, 3, 1]
  -- let player2 = [5, 8, 4, 7, 10]

  let player1 = [47, 19, 22, 31, 24, 6, 10, 5, 1, 48, 46, 27, 8, 45, 16, 28, 33, 41, 42, 36, 50, 39, 30, 11, 17]
  let player2 = [4, 18, 21, 37, 34, 15, 35, 38, 20, 23, 9, 25, 32, 13, 26, 2, 12, 44, 14, 49, 3, 40, 7, 43, 29]

  print $ eval1 $ play1 player1 player2

eval1 :: [Int] -> Int
eval1 xs = sum $ zipWith (*) [1 ..] $ reverse xs

play1 :: [Int] -> [Int] -> [Int]
play1 [] ys = ys
play1 xs [] = xs
play1 (x : xs) (y : ys)
  | x > y = play1 (xs ++ [x, y]) ys
  | otherwise = play1 xs (ys ++ [y, x])
