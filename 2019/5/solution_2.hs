import Control.Monad (guard)
import Data.Sequence
import Debug.Trace (trace)

main :: IO ()
main = do
  print "Solution 2"
  print solution2

puzzleInput :: Seq Int
puzzleInput = fromList [3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1102, 78, 40, 225, 1102, 52, 43, 224, 1001, 224, -2236, 224, 4, 224, 102, 8, 223, 223, 101, 4, 224, 224, 1, 224, 223, 223, 1, 191, 61, 224, 1001, 224, -131, 224, 4, 224, 102, 8, 223, 223, 101, 4, 224, 224, 1, 223, 224, 223, 1101, 86, 74, 225, 1102, 14, 76, 225, 1101, 73, 83, 224, 101, -156, 224, 224, 4, 224, 102, 8, 223, 223, 101, 6, 224, 224, 1, 224, 223, 223, 1102, 43, 82, 225, 2, 196, 13, 224, 101, -6162, 224, 224, 4, 224, 102, 8, 223, 223, 101, 5, 224, 224, 1, 223, 224, 223, 1001, 161, 51, 224, 101, -70, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 1, 224, 1, 224, 223, 223, 102, 52, 187, 224, 1001, 224, -832, 224, 4, 224, 102, 8, 223, 223, 101, 1, 224, 224, 1, 224, 223, 223, 1102, 19, 79, 225, 101, 65, 92, 224, 1001, 224, -147, 224, 4, 224, 1002, 223, 8, 223, 101, 4, 224, 224, 1, 223, 224, 223, 1102, 16, 90, 225, 1102, 45, 44, 225, 1102, 92, 79, 225, 1002, 65, 34, 224, 101, -476, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 5, 224, 1, 224, 223, 223, 4, 223, 99, 0, 0, 0, 677, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105, 1, 99999, 1005, 227, 99999, 1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999, 1106, 0, 265, 1105, 1, 99999, 1006, 0, 99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280, 1105, 1, 99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1, 99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101, 314, 0, 0, 106, 0, 0, 1105, 1, 99999, 107, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 329, 1001, 223, 1, 223, 1007, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 344, 101, 1, 223, 223, 1008, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 359, 1001, 223, 1, 223, 8, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 374, 101, 1, 223, 223, 1107, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 389, 101, 1, 223, 223, 1108, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 404, 101, 1, 223, 223, 107, 677, 677, 224, 102, 2, 223, 223, 1006, 224, 419, 1001, 223, 1, 223, 7, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 434, 101, 1, 223, 223, 1007, 677, 677, 224, 102, 2, 223, 223, 1005, 224, 449, 1001, 223, 1, 223, 108, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 464, 1001, 223, 1, 223, 108, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 479, 101, 1, 223, 223, 107, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 494, 1001, 223, 1, 223, 7, 226, 226, 224, 1002, 223, 2, 223, 1006, 224, 509, 101, 1, 223, 223, 1108, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 524, 101, 1, 223, 223, 1107, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 539, 101, 1, 223, 223, 1008, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 554, 101, 1, 223, 223, 1008, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 569, 101, 1, 223, 223, 1107, 677, 677, 224, 102, 2, 223, 223, 1006, 224, 584, 1001, 223, 1, 223, 1108, 226, 226, 224, 1002, 223, 2, 223, 1006, 224, 599, 101, 1, 223, 223, 7, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 614, 101, 1, 223, 223, 108, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 629, 101, 1, 223, 223, 1007, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 644, 101, 1, 223, 223, 8, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 659, 101, 1, 223, 223, 8, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 674, 101, 1, 223, 223, 4, 223, 99, 226]

solution2 = fst $ ev 0 [5] puzzleInput

example1 x = ev 0 [x] $ fromList [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]

example2 x = ev 0 [x] $ fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]

example3 x = ev 0 [x] $ fromList [3, 3, 1108, -1, 8, 3, 4, 3, 99]

example4 x = ev 0 [x] $ fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99]

example5 x = ev 0 [x] $ fromList [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]

example6 x = ev 0 [x] $ fromList [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]

example7 x = ev 0 [x] $ fromList [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99]

data Op = Stop | Add | Mult | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals deriving (Eq, Show)

parseOp :: Int -> Op
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
    parseOp' 99 = Stop
    parseOp' x = error $ "parseOp " ++ show x

data Mode = Mode Bool Bool Bool deriving (Show)

parseMode :: Int -> Mode
parseMode x =
  let third = x > 10000
      second = x `mod` 10000 > 1000
      first = x `mod` 1000 > 100
   in Mode first second third

type Input = [Int]

type Output = [Int]

ev :: Int -> Input -> Seq Int -> (Output, Seq Int)
ev i input s = ev' (parseOp (s `index` i), i) input s

get :: Bool -> Int -> Seq Int -> Int
get immediateMode i l = if immediateMode then l `index` i else l `index` (l `index` i)

ev' :: (Op, Int) -> Input -> Seq Int -> (Output, Seq Int)
ev' (Stop, _) _ l = ([], l)
ev' (JumpIfTrue, ix) input l =
  let (Mode first second _) = parseMode (l `index` ix)
      s1 = get first (ix + 1) l
      s2 = get second (ix + 2) l
      newIx = if s1 > 0 then s2 else ix + 3
   in ev newIx input l
ev' (JumpIfFalse, ix) input l =
  let (Mode first second _) = parseMode (l `index` ix)
      s1 = get first (ix + 1) l
      s2 = get second (ix + 2) l
      newIx = if s1 == 0 then s2 else ix + 3
   in ev newIx input l
ev' (Input, ix) (x : xs) l =
  let target = l `index` (ix + 1)
   in ev (ix + 2) xs (update target x l)
ev' (Output, ix) i l =
  let (Mode first _ _) = parseMode (l `index` ix)
      valueForOut = get first (ix + 1) l
      (restOut, restSeq) = ev (ix + 2) i l
   in (valueForOut : restOut, restSeq)
ev' (op, ix) input l =
  let (Mode first second third) = parseMode (l `index` ix)
      s1 = get first (ix + 1) l
      s2 = get second (ix + 2) l
      target = l `index` (ix + 3)

      newVal Add = s1 + s2
      newVal Mult = s1 * s2
      newVal LessThan = if s1 < s2 then 1 else 0
      newVal Equals = if s1 == s2 then 1 else 0
      newVal _ = error "WTF"

      nextOp = parseOp (l `index` (ix + 4))
      updatedL = update target (newVal $ parseOp (l `index` ix)) l
      changedIx = (l `index` ix) /= (updatedL `index` ix)
      nextIndex = if changedIx then ix else ix + 4
   in ev nextIndex input updatedL
