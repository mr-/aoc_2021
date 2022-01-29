import Data.List (group, groupBy)

main :: IO ()
main = do
  print "Solution 1"
  print sol1
  print "Solution 2"
  print sol2

input = [146810 .. 612564]

sol1 :: Int
sol1 =
  let valid = filter (check1 . toArray . show) input
   in length valid

sol2 :: Int
sol2 =
  let valid = filter (check2 . toArray . show) input
   in length valid

toArray :: String -> [Int]
toArray = map (\x -> read [x] :: Int)

check1 :: [Int] -> Bool
check1 xs =
  let consPairs = zip xs (tail xs)
      neverDecrease = all (\(a, b) -> a <= b) consPairs
      oneSame = any (\(a, b) -> a == b) consPairs
   in neverDecrease && oneSame

check2 :: [Int] -> Bool
check2 xs =
  let consPairs = zip xs (tail xs)
      neverDecrease = all (\(a, b) -> a <= b) consPairs
      groups = group xs
      oneSame = any (\x -> length x == 2) groups
   in neverDecrease && oneSame
