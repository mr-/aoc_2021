import Control.Applicative (Alternative, (<|>))
import Control.Monad (guard)
import Data.List (maximumBy, permutations)
import Data.Map.Strict
import Debug.Trace (trace)

main :: IO ()
main = do
  print "Solution 1"
  print $ test puzzleInput 1 0 0
  print "Solution 2"
  print $ test puzzleInput 2 0 0

puzzleInput :: Code
puzzleInput = fromList $ zip [0 ..] [1102, 34463338, 34463338, 63, 1007, 63, 34463338, 63, 1005, 63, 53, 1101, 3, 0, 1000, 109, 988, 209, 12, 9, 1000, 209, 6, 209, 3, 203, 0, 1008, 1000, 1, 63, 1005, 63, 65, 1008, 1000, 2, 63, 1005, 63, 904, 1008, 1000, 0, 63, 1005, 63, 58, 4, 25, 104, 0, 99, 4, 0, 104, 0, 99, 4, 17, 104, 0, 99, 0, 0, 1101, 34, 0, 1013, 1101, 20, 0, 1012, 1101, 536, 0, 1023, 1101, 0, 23, 1006, 1102, 1, 543, 1022, 1102, 1, 27, 1003, 1102, 25, 1, 1014, 1102, 1, 29, 1009, 1101, 0, 686, 1025, 1101, 0, 30, 1004, 1102, 1, 28, 1017, 1102, 1, 35, 1016, 1101, 765, 0, 1028, 1102, 1, 33, 1002, 1102, 1, 26, 1000, 1102, 1, 822, 1027, 1102, 1, 21, 1001, 1102, 1, 1, 1021, 1101, 31, 0, 1007, 1101, 0, 39, 1010, 1102, 36, 1, 1019, 1101, 0, 32, 1015, 1101, 0, 38, 1018, 1101, 0, 24, 1005, 1101, 22, 0, 1011, 1101, 756, 0, 1029, 1102, 1, 0, 1020, 1102, 829, 1, 1026, 1102, 1, 37, 1008, 1101, 0, 695, 1024, 109, 19, 1205, 2, 195, 4, 187, 1105, 1, 199, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 7, 1205, -6, 215, 1001, 64, 1, 64, 1105, 1, 217, 4, 205, 1002, 64, 2, 64, 109, -16, 21108, 40, 42, 5, 1005, 1015, 233, 1106, 0, 239, 4, 223, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -13, 2102, 1, 5, 63, 1008, 63, 33, 63, 1005, 63, 261, 4, 245, 1105, 1, 265, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 29, 21101, 41, 0, -9, 1008, 1017, 41, 63, 1005, 63, 291, 4, 271, 1001, 64, 1, 64, 1105, 1, 291, 1002, 64, 2, 64, 109, -22, 2107, 27, -4, 63, 1005, 63, 307, 1105, 1, 313, 4, 297, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 7, 1207, -4, 30, 63, 1005, 63, 333, 1001, 64, 1, 64, 1106, 0, 335, 4, 319, 1002, 64, 2, 64, 109, 1, 21108, 42, 42, 6, 1005, 1018, 353, 4, 341, 1105, 1, 357, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 14, 21101, 43, 0, -7, 1008, 1019, 41, 63, 1005, 63, 377, 1106, 0, 383, 4, 363, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -8, 21102, 44, 1, -1, 1008, 1017, 47, 63, 1005, 63, 407, 1001, 64, 1, 64, 1105, 1, 409, 4, 389, 1002, 64, 2, 64, 109, -15, 2101, 0, 2, 63, 1008, 63, 25, 63, 1005, 63, 433, 1001, 64, 1, 64, 1105, 1, 435, 4, 415, 1002, 64, 2, 64, 109, 7, 1201, -8, 0, 63, 1008, 63, 30, 63, 1005, 63, 455, 1105, 1, 461, 4, 441, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -12, 2108, 37, 10, 63, 1005, 63, 483, 4, 467, 1001, 64, 1, 64, 1106, 0, 483, 1002, 64, 2, 64, 109, 13, 21107, 45, 44, 0, 1005, 1011, 499, 1105, 1, 505, 4, 489, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -2, 2107, 20, -8, 63, 1005, 63, 523, 4, 511, 1106, 0, 527, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 20, 2105, 1, -6, 1001, 64, 1, 64, 1105, 1, 545, 4, 533, 1002, 64, 2, 64, 109, -28, 2102, 1, 1, 63, 1008, 63, 30, 63, 1005, 63, 565, 1105, 1, 571, 4, 551, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 20, 1206, 0, 583, 1105, 1, 589, 4, 577, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -7, 1206, 6, 603, 4, 595, 1106, 0, 607, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -14, 2101, 0, 2, 63, 1008, 63, 33, 63, 1005, 63, 629, 4, 613, 1105, 1, 633, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -4, 1208, 8, 30, 63, 1005, 63, 655, 4, 639, 1001, 64, 1, 64, 1105, 1, 655, 1002, 64, 2, 64, 109, 23, 21107, 46, 47, 0, 1005, 1019, 673, 4, 661, 1105, 1, 677, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -2, 2105, 1, 7, 4, 683, 1001, 64, 1, 64, 1106, 0, 695, 1002, 64, 2, 64, 109, 3, 21102, 47, 1, -7, 1008, 1013, 47, 63, 1005, 63, 717, 4, 701, 1105, 1, 721, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -11, 1202, -7, 1, 63, 1008, 63, 32, 63, 1005, 63, 745, 1001, 64, 1, 64, 1105, 1, 747, 4, 727, 1002, 64, 2, 64, 109, 10, 2106, 0, 9, 4, 753, 1001, 64, 1, 64, 1105, 1, 765, 1002, 64, 2, 64, 109, -24, 1207, 8, 28, 63, 1005, 63, 783, 4, 771, 1106, 0, 787, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, 5, 1201, 0, 0, 63, 1008, 63, 26, 63, 1005, 63, 813, 4, 793, 1001, 64, 1, 64, 1105, 1, 813, 1002, 64, 2, 64, 109, 28, 2106, 0, -1, 1001, 64, 1, 64, 1105, 1, 831, 4, 819, 1002, 64, 2, 64, 109, -22, 1202, -1, 1, 63, 1008, 63, 24, 63, 1005, 63, 857, 4, 837, 1001, 64, 1, 64, 1106, 0, 857, 1002, 64, 2, 64, 109, -9, 2108, 30, 6, 63, 1005, 63, 873, 1106, 0, 879, 4, 863, 1001, 64, 1, 64, 1002, 64, 2, 64, 109, -2, 1208, 10, 26, 63, 1005, 63, 899, 1001, 64, 1, 64, 1106, 0, 901, 4, 885, 4, 64, 99, 21102, 1, 27, 1, 21101, 0, 915, 0, 1105, 1, 922, 21201, 1, 25948, 1, 204, 1, 99, 109, 3, 1207, -2, 3, 63, 1005, 63, 964, 21201, -2, -1, 1, 21101, 942, 0, 0, 1106, 0, 922, 22101, 0, 1, -1, 21201, -2, -3, 1, 21102, 957, 1, 0, 1105, 1, 922, 22201, 1, -1, -2, 1106, 0, 968, 21201, -2, 0, -2, 109, -3, 2106, 0, 0]

puzzleInput5 :: Code
puzzleInput5 = fromList $ zip [0 ..] [3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1102, 78, 40, 225, 1102, 52, 43, 224, 1001, 224, -2236, 224, 4, 224, 102, 8, 223, 223, 101, 4, 224, 224, 1, 224, 223, 223, 1, 191, 61, 224, 1001, 224, -131, 224, 4, 224, 102, 8, 223, 223, 101, 4, 224, 224, 1, 223, 224, 223, 1101, 86, 74, 225, 1102, 14, 76, 225, 1101, 73, 83, 224, 101, -156, 224, 224, 4, 224, 102, 8, 223, 223, 101, 6, 224, 224, 1, 224, 223, 223, 1102, 43, 82, 225, 2, 196, 13, 224, 101, -6162, 224, 224, 4, 224, 102, 8, 223, 223, 101, 5, 224, 224, 1, 223, 224, 223, 1001, 161, 51, 224, 101, -70, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 1, 224, 1, 224, 223, 223, 102, 52, 187, 224, 1001, 224, -832, 224, 4, 224, 102, 8, 223, 223, 101, 1, 224, 224, 1, 224, 223, 223, 1102, 19, 79, 225, 101, 65, 92, 224, 1001, 224, -147, 224, 4, 224, 1002, 223, 8, 223, 101, 4, 224, 224, 1, 223, 224, 223, 1102, 16, 90, 225, 1102, 45, 44, 225, 1102, 92, 79, 225, 1002, 65, 34, 224, 101, -476, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 5, 224, 1, 224, 223, 223, 4, 223, 99, 0, 0, 0, 677, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105, 1, 99999, 1005, 227, 99999, 1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999, 1106, 0, 265, 1105, 1, 99999, 1006, 0, 99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280, 1105, 1, 99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1, 99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101, 314, 0, 0, 106, 0, 0, 1105, 1, 99999, 107, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 329, 1001, 223, 1, 223, 1007, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 344, 101, 1, 223, 223, 1008, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 359, 1001, 223, 1, 223, 8, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 374, 101, 1, 223, 223, 1107, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 389, 101, 1, 223, 223, 1108, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 404, 101, 1, 223, 223, 107, 677, 677, 224, 102, 2, 223, 223, 1006, 224, 419, 1001, 223, 1, 223, 7, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 434, 101, 1, 223, 223, 1007, 677, 677, 224, 102, 2, 223, 223, 1005, 224, 449, 1001, 223, 1, 223, 108, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 464, 1001, 223, 1, 223, 108, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 479, 101, 1, 223, 223, 107, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 494, 1001, 223, 1, 223, 7, 226, 226, 224, 1002, 223, 2, 223, 1006, 224, 509, 101, 1, 223, 223, 1108, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 524, 101, 1, 223, 223, 1107, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 539, 101, 1, 223, 223, 1008, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 554, 101, 1, 223, 223, 1008, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 569, 101, 1, 223, 223, 1107, 677, 677, 224, 102, 2, 223, 223, 1006, 224, 584, 1001, 223, 1, 223, 1108, 226, 226, 224, 1002, 223, 2, 223, 1006, 224, 599, 101, 1, 223, 223, 7, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 614, 101, 1, 223, 223, 108, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 629, 101, 1, 223, 223, 1007, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 644, 101, 1, 223, 223, 8, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 659, 101, 1, 223, 223, 8, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 674, 101, 1, 223, 223, 4, 223, 99, 226]

example1 = fromList $ zip [0 ..] [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]

example2 = fromList $ zip [0 ..] [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]

example_5_1 = fromList $ zip [0 ..] [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]

example_9_1 = fromList $ zip [0 ..] [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]

example_9_2 = fromList $ zip [0 ..] [1102, 34915192, 34915192, 7, 4, 7, 99, 0]

example_9_3 = fromList $ zip [0 ..] [104, 1125899906842624, 99]

--For example, if the relative base is 2000,
--  then after the instruction 109,19, the relative base would be 2019.
-- If the next instruction were 204,-34, then the value at address 1985 would be output.
example_9_4 = fromList [(0, 109), (1, 19), (2, 204), (3, -34), (1985, 42), (4, 99)]

test :: Code -> Integer -> Integer -> Integer -> [Integer]
test code input start rb = evCont [input] (Program code start rb)

data Op = Stop | Add | Mult | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | RBOffset deriving (Eq, Show)

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
   in --  in trace (show ("ix", i, s ?! i, op, rb)) $ ev' (op, i, rb) input s
      ev' (op, i, rb) input s

(?!) :: Map Integer Integer -> Integer -> Integer
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
