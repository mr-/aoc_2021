import Control.Applicative (Alternative, (<|>))
import Control.Monad (guard)
import Data.List (find, maximumBy, permutations)
import Data.Map.Strict hiding (filter, map, take)
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, maybeToList)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)

main :: IO ()
main = do
  print "Solution 1"
  let b = getBoard output
  print $ length $ filter (\(_, i) -> i == "X") b

  print "Solution 2"
  print $ explore (Program puzzleInput2 0 0 Nothing) []
  print "end"

type Board = [((Integer, Integer), [Char])]

explore :: Program -> Output -> Maybe [Char]
explore program output =
  let (state, newOut) = collectOut program
      completeBoard = toList $ fromList $ getBoard (output ++ newOut)
      completeOutput = unGetBoard completeBoard -- dedup the output
      score = snd <$> findScore (getBoard newOut)

      cont (SIn p) = map (\i -> explore p {pInput = Just i} completeOutput) (sortedMoves completeBoard p)
      cont _ = []

      blockCount = length $ filter (\(_, i) -> i == "X") completeBoard
      findScore xs = find (\(c, v) -> c == (-1, 0) && v /= " ") xs

      go 0 _ = score
      go n xs = listToMaybe $ catMaybes xs
   in go blockCount (cont state)

sortedMoves :: Board -> Program -> [Integer]
sortedMoves board program =
  let ballPath = getEndPosition' program []
      ballPos = fst (fromJust (find (\(_, i) -> i == "o") board))
      untilHit = reverse (takeWhile (\(_, y) -> y <= 18) ballPath) ++ [ballPos]
      paddlePos = fst (fromJust (find (\(_, i) -> i == "-") board))

      res GT = [-1, 0, 1]
      res EQ = [0, 1, -1]
      res LT = [1, 0, -1]
   in trace
        (unlines [show (paddlePos, head untilHit), ppBoard board])
        (res (fst paddlePos `compare` fst (head untilHit)))

getEndPosition' :: Program -> [Position] -> [Position]
getEndPosition' program pos = do
  let (state, out) = collectOut program
      ballPos = fst <$> find (\(_, i) -> i == "o") (getBoard out)
      baz old SStop xs = old ++ xs
      baz old (SIn p) xs = getEndPosition' p {pInput = Just 0} (old ++ xs)
      baz _ (SOut _ _) _ = error $ show "baz got out?!"
   in baz pos state (maybeToList ballPos)

puzzleInput :: Code
puzzleInput = fromList $ zip [0 ..] [1, 380, 379, 385, 1008, 2267, 610415, 381, 1005, 381, 12, 99, 109, 2268, 1101, 0, 0, 383, 1101, 0, 0, 382, 20102, 1, 382, 1, 20101, 0, 383, 2, 21101, 37, 0, 0, 1106, 0, 578, 4, 382, 4, 383, 204, 1, 1001, 382, 1, 382, 1007, 382, 37, 381, 1005, 381, 22, 1001, 383, 1, 383, 1007, 383, 22, 381, 1005, 381, 18, 1006, 385, 69, 99, 104, -1, 104, 0, 4, 386, 3, 384, 1007, 384, 0, 381, 1005, 381, 94, 107, 0, 384, 381, 1005, 381, 108, 1105, 1, 161, 107, 1, 392, 381, 1006, 381, 161, 1101, -1, 0, 384, 1106, 0, 119, 1007, 392, 35, 381, 1006, 381, 161, 1101, 0, 1, 384, 21001, 392, 0, 1, 21102, 1, 20, 2, 21101, 0, 0, 3, 21102, 138, 1, 0, 1105, 1, 549, 1, 392, 384, 392, 21002, 392, 1, 1, 21101, 0, 20, 2, 21101, 3, 0, 3, 21101, 161, 0, 0, 1106, 0, 549, 1101, 0, 0, 384, 20001, 388, 390, 1, 20101, 0, 389, 2, 21102, 1, 180, 0, 1105, 1, 578, 1206, 1, 213, 1208, 1, 2, 381, 1006, 381, 205, 20001, 388, 390, 1, 21002, 389, 1, 2, 21101, 205, 0, 0, 1106, 0, 393, 1002, 390, -1, 390, 1102, 1, 1, 384, 21002, 388, 1, 1, 20001, 389, 391, 2, 21102, 228, 1, 0, 1106, 0, 578, 1206, 1, 261, 1208, 1, 2, 381, 1006, 381, 253, 20101, 0, 388, 1, 20001, 389, 391, 2, 21101, 253, 0, 0, 1105, 1, 393, 1002, 391, -1, 391, 1101, 1, 0, 384, 1005, 384, 161, 20001, 388, 390, 1, 20001, 389, 391, 2, 21102, 1, 279, 0, 1105, 1, 578, 1206, 1, 316, 1208, 1, 2, 381, 1006, 381, 304, 20001, 388, 390, 1, 20001, 389, 391, 2, 21101, 304, 0, 0, 1105, 1, 393, 1002, 390, -1, 390, 1002, 391, -1, 391, 1101, 1, 0, 384, 1005, 384, 161, 20102, 1, 388, 1, 20101, 0, 389, 2, 21101, 0, 0, 3, 21102, 338, 1, 0, 1105, 1, 549, 1, 388, 390, 388, 1, 389, 391, 389, 20102, 1, 388, 1, 20101, 0, 389, 2, 21101, 0, 4, 3, 21102, 1, 365, 0, 1106, 0, 549, 1007, 389, 21, 381, 1005, 381, 75, 104, -1, 104, 0, 104, 0, 99, 0, 1, 0, 0, 0, 0, 0, 0, 265, 16, 17, 1, 1, 18, 109, 3, 21202, -2, 1, 1, 21201, -1, 0, 2, 21102, 1, 0, 3, 21102, 414, 1, 0, 1105, 1, 549, 22101, 0, -2, 1, 21202, -1, 1, 2, 21101, 429, 0, 0, 1105, 1, 601, 2102, 1, 1, 435, 1, 386, 0, 386, 104, -1, 104, 0, 4, 386, 1001, 387, -1, 387, 1005, 387, 451, 99, 109, -3, 2106, 0, 0, 109, 8, 22202, -7, -6, -3, 22201, -3, -5, -3, 21202, -4, 64, -2, 2207, -3, -2, 381, 1005, 381, 492, 21202, -2, -1, -1, 22201, -3, -1, -3, 2207, -3, -2, 381, 1006, 381, 481, 21202, -4, 8, -2, 2207, -3, -2, 381, 1005, 381, 518, 21202, -2, -1, -1, 22201, -3, -1, -3, 2207, -3, -2, 381, 1006, 381, 507, 2207, -3, -4, 381, 1005, 381, 540, 21202, -4, -1, -1, 22201, -3, -1, -3, 2207, -3, -4, 381, 1006, 381, 529, 22101, 0, -3, -7, 109, -8, 2106, 0, 0, 109, 4, 1202, -2, 37, 566, 201, -3, 566, 566, 101, 639, 566, 566, 2102, 1, -1, 0, 204, -3, 204, -2, 204, -1, 109, -4, 2105, 1, 0, 109, 3, 1202, -1, 37, 593, 201, -2, 593, 593, 101, 639, 593, 593, 21001, 0, 0, -2, 109, -3, 2105, 1, 0, 109, 3, 22102, 22, -2, 1, 22201, 1, -1, 1, 21102, 1, 409, 2, 21102, 1, 463, 3, 21102, 1, 814, 4, 21102, 1, 630, 0, 1106, 0, 456, 21201, 1, 1453, -2, 109, -3, 2105, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 0, 2, 0, 0, 0, 0, 1, 1, 0, 0, 2, 0, 0, 2, 2, 0, 0, 0, 2, 2, 0, 2, 2, 0, 0, 0, 2, 2, 0, 2, 2, 2, 2, 0, 0, 2, 2, 0, 0, 2, 0, 2, 0, 1, 1, 0, 0, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 0, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0, 1, 1, 0, 2, 0, 0, 2, 2, 2, 0, 2, 2, 0, 2, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 0, 2, 2, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 1, 1, 0, 2, 2, 2, 0, 0, 0, 2, 2, 2, 0, 2, 2, 2, 2, 2, 0, 0, 0, 2, 2, 0, 2, 2, 2, 0, 2, 0, 0, 0, 0, 0, 2, 2, 0, 1, 1, 0, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 0, 0, 0, 0, 2, 0, 0, 2, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 0, 1, 1, 0, 0, 0, 2, 0, 0, 2, 2, 2, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0, 2, 0, 0, 2, 0, 2, 2, 2, 0, 1, 1, 0, 2, 0, 0, 2, 2, 0, 0, 0, 2, 0, 0, 0, 2, 2, 2, 2, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 1, 1, 0, 2, 0, 0, 0, 2, 0, 2, 2, 2, 2, 2, 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 0, 2, 2, 0, 2, 2, 0, 2, 0, 0, 2, 2, 0, 1, 1, 0, 2, 0, 2, 2, 0, 2, 2, 0, 0, 0, 0, 0, 2, 2, 0, 2, 0, 0, 0, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 1, 1, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0, 2, 0, 0, 2, 2, 0, 2, 0, 0, 0, 1, 1, 0, 0, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 2, 0, 0, 2, 2, 0, 2, 2, 2, 0, 0, 0, 0, 2, 0, 2, 2, 0, 2, 0, 2, 2, 0, 1, 1, 0, 0, 2, 2, 2, 2, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 0, 0, 0, 0, 2, 0, 2, 2, 2, 0, 2, 0, 0, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 2, 2, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 2, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 72, 10, 67, 45, 58, 25, 55, 73, 97, 49, 19, 51, 58, 95, 30, 82, 74, 9, 98, 96, 38, 64, 30, 45, 14, 73, 42, 5, 3, 61, 68, 23, 18, 14, 9, 16, 21, 7, 77, 39, 38, 16, 82, 17, 58, 87, 90, 64, 52, 1, 96, 67, 66, 16, 65, 15, 22, 41, 69, 90, 93, 92, 96, 45, 68, 17, 63, 51, 15, 61, 51, 93, 65, 55, 42, 76, 48, 52, 31, 98, 6, 88, 69, 65, 65, 30, 51, 88, 4, 13, 36, 90, 80, 23, 31, 42, 63, 86, 52, 15, 79, 78, 59, 77, 57, 71, 84, 81, 73, 56, 1, 5, 7, 86, 75, 31, 63, 76, 21, 73, 16, 41, 86, 15, 78, 85, 2, 79, 63, 54, 79, 65, 87, 13, 86, 96, 81, 69, 27, 76, 8, 48, 5, 79, 10, 74, 76, 86, 95, 55, 72, 52, 23, 41, 50, 46, 68, 29, 86, 61, 96, 29, 34, 40, 86, 86, 1, 20, 90, 35, 69, 64, 50, 51, 75, 65, 93, 19, 5, 15, 96, 3, 88, 8, 43, 66, 88, 72, 84, 69, 42, 4, 95, 51, 80, 81, 27, 75, 92, 22, 45, 54, 63, 51, 82, 91, 13, 25, 54, 41, 84, 84, 29, 98, 50, 91, 11, 40, 69, 13, 47, 42, 72, 46, 87, 31, 27, 98, 65, 94, 26, 51, 79, 39, 29, 38, 42, 46, 25, 36, 26, 66, 12, 93, 58, 1, 61, 41, 37, 57, 60, 60, 9, 70, 63, 26, 56, 1, 27, 5, 11, 93, 17, 48, 95, 19, 79, 16, 14, 16, 29, 79, 56, 16, 26, 37, 50, 10, 38, 53, 4, 10, 3, 57, 20, 59, 16, 51, 88, 66, 74, 91, 56, 42, 84, 30, 36, 31, 36, 58, 68, 66, 91, 36, 71, 30, 39, 96, 50, 84, 76, 95, 14, 89, 75, 59, 77, 66, 36, 88, 62, 60, 3, 45, 13, 39, 48, 33, 59, 21, 19, 35, 90, 81, 66, 52, 75, 34, 70, 55, 56, 47, 22, 20, 87, 73, 73, 76, 73, 8, 96, 55, 46, 5, 1, 64, 27, 8, 37, 87, 50, 8, 79, 74, 63, 26, 43, 44, 2, 85, 91, 28, 13, 16, 15, 55, 87, 94, 28, 86, 66, 29, 34, 46, 18, 41, 37, 94, 63, 31, 78, 48, 17, 4, 25, 62, 15, 10, 18, 19, 97, 50, 78, 5, 79, 5, 70, 64, 86, 61, 58, 59, 61, 5, 71, 68, 14, 24, 17, 56, 85, 52, 64, 92, 45, 90, 94, 55, 47, 5, 56, 59, 20, 15, 41, 36, 58, 55, 25, 47, 45, 69, 58, 36, 44, 80, 94, 52, 84, 17, 27, 20, 44, 51, 93, 10, 56, 77, 45, 29, 93, 63, 96, 95, 47, 31, 63, 69, 64, 74, 53, 34, 36, 20, 14, 40, 30, 61, 86, 15, 3, 94, 61, 43, 75, 59, 64, 41, 34, 98, 32, 65, 73, 18, 30, 46, 66, 38, 68, 25, 96, 16, 37, 54, 38, 44, 26, 52, 1, 2, 21, 93, 37, 26, 4, 45, 69, 82, 59, 34, 55, 34, 77, 88, 46, 70, 32, 56, 82, 10, 20, 31, 40, 20, 55, 3, 3, 93, 95, 65, 56, 61, 68, 41, 35, 62, 20, 58, 55, 42, 41, 40, 33, 51, 6, 52, 84, 27, 62, 81, 32, 35, 87, 97, 79, 7, 97, 77, 40, 48, 74, 4, 6, 36, 58, 59, 25, 6, 5, 84, 7, 44, 51, 88, 37, 9, 30, 29, 26, 91, 41, 72, 39, 24, 68, 58, 49, 80, 49, 43, 98, 43, 92, 9, 49, 64, 10, 96, 50, 86, 56, 2, 72, 58, 80, 57, 77, 61, 74, 14, 42, 50, 55, 40, 21, 77, 20, 19, 16, 80, 84, 92, 27, 32, 37, 80, 59, 69, 13, 11, 19, 6, 94, 54, 88, 51, 69, 41, 54, 68, 36, 82, 68, 19, 77, 85, 37, 5, 58, 61, 72, 5, 67, 17, 35, 29, 18, 71, 46, 5, 29, 8, 93, 97, 36, 37, 25, 93, 27, 33, 93, 79, 10, 84, 75, 6, 91, 98, 34, 32, 37, 70, 18, 84, 52, 32, 11, 88, 44, 69, 58, 92, 52, 68, 77, 39, 90, 9, 58, 74, 1, 53, 56, 64, 75, 46, 59, 39, 52, 32, 41, 62, 81, 75, 7, 93, 29, 89, 51, 34, 31, 93, 70, 94, 30, 98, 68, 3, 60, 2, 2, 49, 31, 15, 65, 11, 78, 70, 2, 50, 29, 9, 9, 85, 65, 52, 28, 95, 55, 77, 98, 29, 65, 56, 51, 32, 44, 42, 82, 14, 29, 22, 5, 29, 65, 86, 84, 88, 58, 63, 10, 13, 13, 51, 97, 17, 57, 19, 39, 83, 72, 93, 15, 54, 31, 83, 3, 43, 21, 83, 74, 2, 86, 47, 25, 89, 20, 11, 68, 80, 29, 21, 58, 69, 610415]

puzzleInput2 = insert 0 2 puzzleInput

output = evCont (Program puzzleInput 0 0 Nothing)

-- 0 is an empty tile. No game object appears in this tile.
-- 1 is a wall tile. Walls are indestructible barriers.
-- 2 is a block tile. Blocks can be broken by the ball.
-- 3 is a horizontal paddle tile. The paddle is indestructible.
-- 4 is a ball tile. The ball moves diagonally and bounces off objects.

getBoard :: [Integer] -> Board
getBoard (x : y : id : xs) = ((x, y), c id) : getBoard xs
  where
    c 0 = " "
    c 1 = "#"
    c 2 = "X"
    c 3 = "-"
    c 4 = "o"
    c n = show n
getBoard [] = []
getBoard xs = error $ show ("WTF", xs)

unGetBoard :: Board -> [Integer]
unGetBoard (((x, y), id) : xs) = x : y : c id : unGetBoard xs
  where
    c " " = 0
    c "#" = 1
    c "X" = 2
    c "-" = 3
    c "o" = 4
    c n = read n
unGetBoard [] = []

data Op = Stop | Add | Mult | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | RBOffset deriving (Eq, Show)

type Position = (Integer, Integer)

type Direction = (Integer, Integer)

type Hull = Map Position Integer

iterateAlt :: (Alternative f, Monad f) => (a -> f a) -> a -> f a
iterateAlt f = go where go x = (f x >>= go) <|> return x

parseOp :: Integer -> Op
parseOp x = parseOp' (x `mod` 100)
  where
    parseOp' 1 = Add
    parseOp' 2 = Mult
    parseOp' 3 = Input
    parseOp' 4 = Output
    parseOp' 5 = JumpIfTrue
    parseOp' 6 = JumpIfFalse
    parseOp' 7 = LessThan
    parseOp' 8 = Equals
    parseOp' 9 = RBOffset
    parseOp' 99 = Stop
    parseOp' x = error $ "parseOp " ++ show x

data ParameterMode = Relative | Position | Immediate deriving (Show, Eq)

data Modes = Modes ParameterMode ParameterMode ParameterMode deriving (Show)

parseMode :: Integer -> Modes
parseMode x =
  let third = case (x > 10000, x > 20000) of
        (True, True) -> Relative
        (True, False) -> Immediate
        _ -> Position
      second = case (x `mod` 10000 > 1000, x `mod` 10000 > 2000) of
        (True, True) -> Relative
        (True, False) -> Immediate
        _ -> Position
      first = case (x `mod` 1000 > 100, x `mod` 1000 > 200) of
        (True, True) -> Relative
        (True, False) -> Immediate
        _ -> Position
   in Modes first second third

type Code = Map Integer Integer

type Input = [Integer]

type Output = [Integer]

-- Maybe Integer as input is a bit of hack.
-- if it is not set, the program returns with Sin and can be continued by setting pInput
data Program = Program {pCode :: Code, pIdx :: Integer, pRelBase :: Integer, pInput :: Maybe Integer}

evCont :: Program -> [Integer]
evCont = go
  where
    go y =
      let res = ev y
       in case res of
            SStop -> []
            SOut p out -> out : go p
            SIn p -> go p {pInput = Just 0}

collectOut :: Program -> (State, [Integer])
collectOut p = go p []
  where
    go y out =
      let res = ev y
       in case res of
            SStop -> (SStop, out)
            SOut p o -> go p (out ++ [o])
            SIn p -> (SIn p, out)

-- Nothing means the program has stopped, Just wants to continue here.
data State = SOut Program Integer | SIn Program | SStop

ev :: Program -> State
ev (Program s i rb input) =
  let op = parseOp (s ?! i)
   in ev' (op, i, rb) input s

(?!) :: Ord b => Map b Integer -> b -> Integer
l ?! idx = findWithDefault 0 idx l

get :: ParameterMode -> Integer -> Integer -> Code -> Integer
get Position idx rb l = l ?! (l ?! idx)
get Immediate idx rb l = l ?! idx
get Relative idx rb l = l ?! ((l ?! idx) + rb)

ev' :: (Op, Integer, Integer) -> Maybe Integer -> Code -> State
ev' (Stop, _, _) _ l = SStop
ev' (JumpIfTrue, ix, rb) input l =
  let (Modes first second _) = parseMode (l ?! ix)
      s1 = get first (ix + 1) rb l
      s2 = get second (ix + 2) rb l
      newIx = if s1 > 0 then s2 else ix + 3
   in ev (Program l newIx rb input)
ev' (JumpIfFalse, ix, rb) input l =
  let (Modes first second _) = parseMode (l ?! ix)
      s1 = get first (ix + 1) rb l
      s2 = get second (ix + 2) rb l
      newIx = if s1 == 0 then s2 else ix + 3
   in ev (Program l newIx rb input)
ev' (Input, ix, rb) Nothing l =
  SIn (Program l ix rb Nothing)
ev' (Input, ix, rb) (Just x) l =
  let (Modes first _ _) = parseMode (l ?! ix)
      target = if first == Relative then (l ?! (ix + 1)) + rb else l ?! (ix + 1)
   in ev (Program (insert target x l) (ix + 2) rb Nothing)
ev' (RBOffset, ix, rb) input l =
  let (Modes first _ _) = parseMode (l ?! ix)
      val = get first (ix + 1) rb l
   in ev (Program l (ix + 2) (rb + val) input)
ev' (Output, ix, rb) input l =
  let (Modes first _ _) = parseMode (l ?! ix)
      valueForOut = get first (ix + 1) rb l
   in SOut (Program l (ix + 2) rb input) valueForOut
ev' (op, ix, rb) input l =
  let (Modes first second third) = parseMode (l ?! ix)
      s1 = get first (ix + 1) rb l
      s2 = get second (ix + 2) rb l
      target = if third == Relative then (l ?! (ix + 3)) + rb else l ?! (ix + 3)

      newVal Add = s1 + s2
      newVal Mult = s1 * s2
      newVal LessThan = if s1 < s2 then 1 else 0
      newVal Equals = if s1 == s2 then 1 else 0
      newVal x = error $ show ("newVal WTF", x)

      updatedL = insert target (newVal $ parseOp (l ?! ix)) l
      changedIx = (l ?! ix) /= (updatedL ?! ix)
      nextIndex = if changedIx then ix else ix + 4
   in ev (Program updatedL nextIndex rb input)

ppBoard :: [((Integer, Integer), String)] -> String
ppBoard boardList =
  let boardMap = fromList boardList
      xs = [fst $ fst p | p <- boardList]
      ys = [snd $ fst p | p <- boardList]
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)
      c x y = if member (x, y) boardMap then boardMap ! (x, y) else " "
   in unlines $ [concat [c x y | x <- [0 .. maxX]] | y <- [0 .. maxY]]
