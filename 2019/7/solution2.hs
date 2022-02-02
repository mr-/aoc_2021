import Control.Applicative (Alternative, (<|>))
import Control.Monad (guard)
import Data.List (maximumBy, permutations)
import Data.Sequence
import Debug.Trace (trace)

main :: IO ()
main = do
  print "Solution 2"
  print $ find example1
  print $ find example2
  print $ find puzzleInput

find ex = maximumBy (\(a, b) (c, d) -> b `compare` d) $ findAll ex

findAll ex = do
  xs@[a, b, c, d, e] <- permutations [5, 6, 7, 8, 9]
  let Just (_, o) = iterateAlt eval ([(ex, 0), (ex, 0), (ex, 0), (ex, 0), (ex, 0)], [[a, 0], [b], [c], [d], [e]])
  return (xs, head o)

iterateAlt :: (Alternative f, Monad f) => (a -> f a) -> a -> f a
iterateAlt f = go where go x = (f x >>= go) <|> return x

eval :: ([Program], [Input]) -> Maybe ([Program], [[Int]])
eval ([p0, p1, p2, p3, p4], [ph0, ph1, ph2, ph3, ph4]) = do
  (p0', o0) <- ev ph0 p0
  (p1', o1) <- ev (ph1 ++ [o0]) p1
  (p2', o2) <- ev (ph2 ++ [o1]) p2
  (p3', o3) <- ev (ph3 ++ [o2]) p3
  (p4', o4) <- ev (ph4 ++ [o3]) p4

  return ([p0', p1', p2', p3', p4'], [[o4], [], [], [], []])
eval (x, y) = error "well.. just to make the exhaustiveness checker happy.."

puzzleInput :: Seq Int
puzzleInput = fromList [3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 34, 43, 64, 85, 98, 179, 260, 341, 422, 99999, 3, 9, 1001, 9, 3, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 102, 5, 9, 9, 4, 9, 99, 3, 9, 1001, 9, 2, 9, 1002, 9, 4, 9, 1001, 9, 3, 9, 1002, 9, 4, 9, 4, 9, 99, 3, 9, 1001, 9, 3, 9, 102, 3, 9, 9, 101, 4, 9, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 1002, 9, 3, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 99]

example1 = fromList [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]

example2 = fromList [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]

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

type Program = (Seq Int, Int)

-- Nothing means the program has stopped, Just wants to continue here.
type State = Maybe (Program, Int)

ev :: Input -> Program -> State
ev input (s, i) = ev' (parseOp (s `index` i), i) input s

get :: Bool -> Int -> Seq Int -> Int
get immediateMode i l = if immediateMode then l `index` i else l `index` (l `index` i)

ev' :: (Op, Int) -> Input -> Seq Int -> State
ev' (Stop, _) _ l = Nothing
ev' (JumpIfTrue, ix) input l =
  let (Mode first second _) = parseMode (l `index` ix)
      s1 = get first (ix + 1) l
      s2 = get second (ix + 2) l
      newIx = if s1 > 0 then s2 else ix + 3
   in ev input (l, newIx)
ev' (JumpIfFalse, ix) input l =
  let (Mode first second _) = parseMode (l `index` ix)
      s1 = get first (ix + 1) l
      s2 = get second (ix + 2) l
      newIx = if s1 == 0 then s2 else ix + 3
   in ev input (l, newIx)
ev' (Input, ix) (x : xs) l =
  let target = l `index` (ix + 1)
   in ev xs (update target x l, ix + 2)
ev' (Output, ix) i l =
  let (Mode first _ _) = parseMode (l `index` ix)
      valueForOut = get first (ix + 1) l
   in Just ((l, ix + 2), valueForOut)
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
   in ev input (updatedL, nextIndex)
