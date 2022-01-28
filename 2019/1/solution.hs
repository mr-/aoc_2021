main :: IO ()
main = do
  ls <- map (read :: String -> Int) <$> lines <$> readFile "input.txt"

  print "Solution 1"
  print $ sum $ map getFuel ls

  print "Solution 1"
  print $ sum $ map getTotalFuel ls

getTotalFuel :: Int -> Int
getTotalFuel mass =
  let sequence = drop 1 $ iterate getFuel mass
      res = sum $ takeWhile (> 0) sequence
   in res

-- take its mass, divide by three, round down, and subtract 2.
getFuel :: Int -> Int
getFuel x = (x `div` 3) - 2
