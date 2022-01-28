import Control.Monad (guard)
import Data.Sequence
import Debug.Trace (trace)

main :: IO ()
main = do
  print "Solution 1"
  print solution1

  print "Solution 2"
  print solution2

-- replace position 1 with the value 12 and replace position 2 with the value 2
puzzleInput :: Seq Int
puzzleInput = update 2 2 $ update 1 12 $ fromList [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 9, 1, 19, 1, 9, 19, 23, 1, 23, 5, 27, 2, 27, 10, 31, 1, 6, 31, 35, 1, 6, 35, 39, 2, 9, 39, 43, 1, 6, 43, 47, 1, 47, 5, 51, 1, 51, 13, 55, 1, 55, 13, 59, 1, 59, 5, 63, 2, 63, 6, 67, 1, 5, 67, 71, 1, 71, 13, 75, 1, 10, 75, 79, 2, 79, 6, 83, 2, 9, 83, 87, 1, 5, 87, 91, 1, 91, 5, 95, 2, 9, 95, 99, 1, 6, 99, 103, 1, 9, 103, 107, 2, 9, 107, 111, 1, 111, 6, 115, 2, 9, 115, 119, 1, 119, 6, 123, 1, 123, 9, 127, 2, 127, 13, 131, 1, 131, 9, 135, 1, 10, 135, 139, 2, 139, 10, 143, 1, 143, 5, 147, 2, 147, 6, 151, 1, 151, 5, 155, 1, 2, 155, 159, 1, 6, 159, 0, 99, 2, 0, 14, 0]

exampleInput :: Seq Int
exampleInput = fromList [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

s1 :: Seq Int
s1 = fromList [1, 0, 0, 0, 99]

s2 :: Seq Int
s2 = fromList [2, 3, 0, 3, 99]

s3 :: Seq Int
s3 = fromList [2, 4, 4, 5, 99, 0]

s4 :: Seq Int
s4 = fromList [1, 1, 1, 4, 99, 5, 6, 0, 99]

solution1 = ev 0 puzzleInput `index` 0

solution2 =
  let (x, y, _) = head find
   in 100 * x + y

find :: [(Int, Int, Int)]
find = do
  x <- [1 .. 99]
  y <- [1 .. 99]
  let newProgram = update 1 x $ update 2 y puzzleInput
  let res = ev 0 newProgram `index` 0
  guard $ res == 19690720
  return (x, y, res)

ev :: Int -> Seq Int -> Seq Int
-- ev ix l = trace (show [show ix, show l, show [ix, ix + 1, ix + 2, ix + 3]]) $ evaluate ix l
ev = evaluate

evaluate :: Int -> Seq Int -> Seq Int
evaluate ix l
  | l `index` ix == 99 = l
  | otherwise =
    let s1 = l `index` (l `index` (ix + 1))
        s2 = l `index` (l `index` (ix + 2))
        target = l `index` (ix + 3)

        newVal 1 = s1 + s2
        newVal 2 = s1 * s2
        newVal _ = error "WTF"
     in ev (ix + 4) (update target (newVal (l `index` ix)) l)
