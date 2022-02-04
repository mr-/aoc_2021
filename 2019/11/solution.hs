import Control.Applicative (Alternative, (<|>))
import Control.Monad (guard)
import Data.List (maximumBy, permutations)
import Data.Map.Strict
import Debug.Trace (trace)

main :: IO ()
main = do
  print "Solution 1"
  print sol1

puzzleInput :: Code
puzzleInput = fromList $ zip [0 ..] [3, 8, 1005, 8, 302, 1106, 0, 11, 0, 0, 0, 104, 1, 104, 0, 3, 8, 102, -1, 8, 10, 101, 1, 10, 10, 4, 10, 1008, 8, 0, 10, 4, 10, 101, 0, 8, 29, 1006, 0, 78, 2, 1007, 9, 10, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 1008, 8, 1, 10, 4, 10, 1002, 8, 1, 58, 1006, 0, 7, 3, 8, 1002, 8, -1, 10, 101, 1, 10, 10, 4, 10, 1008, 8, 0, 10, 4, 10, 1002, 8, 1, 83, 2, 1009, 4, 10, 3, 8, 102, -1, 8, 10, 1001, 10, 1, 10, 4, 10, 1008, 8, 0, 10, 4, 10, 1002, 8, 1, 109, 1, 106, 11, 10, 1006, 0, 16, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 1008, 8, 1, 10, 4, 10, 102, 1, 8, 138, 2, 108, 0, 10, 1, 101, 14, 10, 1, 1109, 1, 10, 3, 8, 1002, 8, -1, 10, 101, 1, 10, 10, 4, 10, 1008, 8, 0, 10, 4, 10, 102, 1, 8, 172, 2, 3, 10, 10, 1006, 0, 49, 3, 8, 1002, 8, -1, 10, 101, 1, 10, 10, 4, 10, 1008, 8, 1, 10, 4, 10, 1001, 8, 0, 201, 1006, 0, 28, 2, 3, 15, 10, 2, 109, 12, 10, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 108, 0, 8, 10, 4, 10, 1001, 8, 0, 233, 3, 8, 102, -1, 8, 10, 1001, 10, 1, 10, 4, 10, 108, 1, 8, 10, 4, 10, 101, 0, 8, 255, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 108, 1, 8, 10, 4, 10, 102, 1, 8, 277, 2, 1107, 9, 10, 101, 1, 9, 9, 1007, 9, 946, 10, 1005, 10, 15, 99, 109, 624, 104, 0, 104, 1, 21101, 0, 932856042280, 1, 21101, 0, 319, 0, 1105, 1, 423, 21101, 0, 387512640296, 1, 21101, 330, 0, 0, 1106, 0, 423, 3, 10, 104, 0, 104, 1, 3, 10, 104, 0, 104, 0, 3, 10, 104, 0, 104, 1, 3, 10, 104, 0, 104, 1, 3, 10, 104, 0, 104, 0, 3, 10, 104, 0, 104, 1, 21101, 0, 46266346499, 1, 21102, 1, 377, 0, 1105, 1, 423, 21102, 1, 46211836967, 1, 21102, 1, 388, 0, 1105, 1, 423, 3, 10, 104, 0, 104, 0, 3, 10, 104, 0, 104, 0, 21102, 1, 825460941588, 1, 21102, 411, 1, 0, 1106, 0, 423, 21101, 709475738388, 0, 1, 21102, 1, 422, 0, 1105, 1, 423, 99, 109, 2, 21201, -1, 0, 1, 21101, 0, 40, 2, 21102, 454, 1, 3, 21101, 0, 444, 0, 1106, 0, 487, 109, -2, 2106, 0, 0, 0, 1, 0, 0, 1, 109, 2, 3, 10, 204, -1, 1001, 449, 450, 465, 4, 0, 1001, 449, 1, 449, 108, 4, 449, 10, 1006, 10, 481, 1102, 1, 0, 449, 109, -2, 2106, 0, 0, 0, 109, 4, 2102, 1, -1, 486, 1207, -3, 0, 10, 1006, 10, 504, 21101, 0, 0, -3, 22101, 0, -3, 1, 21201, -2, 0, 2, 21102, 1, 1, 3, 21102, 1, 523, 0, 1105, 1, 528, 109, -4, 2105, 1, 0, 109, 5, 1207, -3, 1, 10, 1006, 10, 551, 2207, -4, -2, 10, 1006, 10, 551, 22101, 0, -4, -4, 1105, 1, 619, 22102, 1, -4, 1, 21201, -3, -1, 2, 21202, -2, 2, 3, 21101, 570, 0, 0, 1106, 0, 528, 22102, 1, 1, -4, 21102, 1, 1, -1, 2207, -4, -2, 10, 1006, 10, 589, 21101, 0, 0, -1, 22202, -2, -1, -2, 2107, 0, -3, 10, 1006, 10, 611, 21201, -1, 0, 1, 21101, 611, 0, 0, 106, 0, 486, 21202, -2, -1, -2, 22201, -4, -2, -4, 109, -5, 2105, 1, 0]

data Op = Stop | Add | Mult | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | RBOffset deriving (Eq, Show)

type Position = (Integer, Integer)

type Direction = (Integer, Integer)

type Hull = Map Position Integer

iterateAlt :: (Alternative f, Monad f) => (a -> f a) -> a -> f a
iterateAlt f = go where go x = (f x >>= go) <|> return x

sol1 :: Int
sol1 =
  let p = Program puzzleInput 0 0
      (Just (_, _, _, hull)) = iterateAlt doThing (p, (0, 0), (0, 1), empty)
   in size hull

doThing :: (Program, Position, Direction, Hull) -> Maybe (Program, Position, Direction, Hull)
doThing (program, position, direction, hull) = do
  let currentColor = hull ?! position
  (program', newColor) <- ev [currentColor] program
  (program'', turn) <- ev [currentColor] program'
  let hull' = insert position newColor hull
  let rotate 0 (a, b) = (- b, a)
      rotate 1 (a, b) = (b, - a)
      rotate n p = error $ show ("rotate", n, p)
      add (a, b) (c, d) = (a + c, b + d)

  let direction' = rotate turn direction
      position' = add position direction'
  -- rotate direction and add it to position
  return (program'', position', direction', hull')

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

data Program = Program {pCode :: Code, pIdx :: Integer, pRelBase :: Integer}

evCont :: [Integer] -> Program -> [Integer]
evCont input = go
  where
    go y =
      let res = ev input y
       in case res of
            Nothing -> []
            Just (p, out) -> out : go p

-- Nothing means the program has stopped, Just wants to continue here.
type State = Maybe (Program, Integer)

ev :: Input -> Program -> State
ev input (Program s i rb) =
  let op = parseOp (s ?! i)
   in ev' (op, i, rb) input s

(?!) :: Ord b => Map b Integer -> b -> Integer
l ?! idx = findWithDefault 0 idx l

get :: ParameterMode -> Integer -> Integer -> Code -> Integer
get Position idx rb l = l ?! (l ?! idx)
get Immediate idx rb l = l ?! idx
get Relative idx rb l = l ?! ((l ?! idx) + rb)

ev' :: (Op, Integer, Integer) -> Input -> Code -> State
ev' (Stop, _, _) _ l = Nothing
ev' (JumpIfTrue, ix, rb) input l =
  let (Modes first second _) = parseMode (l ?! ix)
      s1 = get first (ix + 1) rb l
      s2 = get second (ix + 2) rb l
      newIx = if s1 > 0 then s2 else ix + 3
   in ev input (Program l newIx rb)
ev' (JumpIfFalse, ix, rb) input l =
  let (Modes first second _) = parseMode (l ?! ix)
      s1 = get first (ix + 1) rb l
      s2 = get second (ix + 2) rb l
      newIx = if s1 == 0 then s2 else ix + 3
   in ev input (Program l newIx rb)
ev' (Input, ix, rb) (x : xs) l =
  let (Modes first _ _) = parseMode (l ?! ix)
      target = if first == Relative then (l ?! (ix + 1)) + rb else l ?! (ix + 1)
   in ev xs (Program (insert target x l) (ix + 2) rb)
ev' (RBOffset, ix, rb) input l =
  let (Modes first _ _) = parseMode (l ?! ix)
      val = get first (ix + 1) rb l
   in ev input (Program l (ix + 2) (rb + val))
ev' (Output, ix, rb) i l =
  let (Modes first _ _) = parseMode (l ?! ix)
      valueForOut = get first (ix + 1) rb l
   in Just (Program l (ix + 2) rb, valueForOut)
ev' (op, ix, rb) input l =
  let (Modes first second third) = parseMode (l ?! ix)
      s1 = get first (ix + 1) rb l
      s2 = get second (ix + 2) rb l
      target = if third == Relative then (l ?! (ix + 3)) + rb else l ?! (ix + 3)

      newVal Add = s1 + s2
      newVal Mult = s1 * s2
      newVal LessThan = if s1 < s2 then 1 else 0
      newVal Equals = if s1 == s2 then 1 else 0
      newVal _ = error "WTF"

      updatedL = insert target (newVal $ parseOp (l ?! ix)) l
      changedIx = (l ?! ix) /= (updatedL ?! ix)
      nextIndex = if changedIx then ix else ix + 4
   in ev input (Program updatedL nextIndex rb)
