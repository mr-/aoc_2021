import Data.List (elemIndex)
import Maybes (fromJust)

main = do
  let tr = transforms 7
  let doorLoopSize = fromJust $ elemIndex door tr
  let cardLoopSize = fromJust $ elemIndex card tr
  print ["doorLoopSize: ", show doorLoopSize, "cardLoopSize ", show cardLoopSize]
  let trDoor = transforms door
  print $ trDoor !! cardLoopSize

sampleDoor = 5764801

sampleCard = 17807724

door = 16915772

card = 18447943

transforms :: Int -> [Int]
transforms subj = iterate (transform subj) 1

transform :: Int -> Int -> Int
transform subj v = (v * subj) `mod` 20201227
