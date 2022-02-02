import Control.Monad (guard)
import Data.List (maximumBy, permutations)
import Data.Sequence
import Debug.Trace (trace)

main :: IO ()
main = do
  print "Solution 1"
  print $ evaluate example1 [4, 3, 2, 1, 0]
  print $ evaluate example2 [0, 1, 2, 3, 4]
  print $ evaluate example3 [1, 0, 4, 3, 2]
  print $ maximumBy (\(a, b) (c, d) -> b `compare` d) $ find example1
  print $ maximumBy (\(a, b) (c, d) -> b `compare` d) $ find example2
  print $ maximumBy (\(a, b) (c, d) -> b `compare` d) $ find example3
  print $ maximumBy (\(a, b) (c, d) -> b `compare` d) $ find puzzleInput

find program = do
  xs@[a, b, c, d, e] <- permutations [0, 1, 2, 3, 4]
  return (xs, evaluate program xs)

evaluate :: Seq Int -> [Int] -> Int
evaluate program [p0, p1, p2, p3, p4] =
  let (o0, _) = ev 0 [p0, 0] program
      (o1, _) = ev 0 [p1, head o0] program
      (o2, _) = ev 0 [p2, head o1] program
      (o3, _) = ev 0 [p3, head o2] program
      (o4, _) = ev 0 [p4, head o3] program
   in head o4
evaluate program xs = undefined

puzzleInput :: Seq Int
puzzleInput = fromList [3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 34, 43, 64, 85, 98, 179, 260, 341, 422, 99999, 3, 9, 1001, 9, 3, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 102, 5, 9, 9, 4, 9, 99, 3, 9, 1001, 9, 2, 9, 1002, 9, 4, 9, 1001, 9, 3, 9, 1002, 9, 4, 9, 4, 9, 99, 3, 9, 1001, 9, 3, 9, 102, 3, 9, 9, 101, 4, 9, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 1002, 9, 3, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 99]

example1 = fromList [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]

example2 = fromList [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]

example3 = fromList [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]

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
