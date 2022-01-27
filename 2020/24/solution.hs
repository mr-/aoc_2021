import qualified Control.Applicative as A
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.VectorSpace
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    choice,
    many,
    optional,
    parse,
    parseMaybe,
    parseTest,
    skipSome,
    some,
  )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

data Dir = E | SE | SW | W | NW | NE deriving (Show)

-- This cube representation is shamelessly taken from https://www.redblobgames.com/grids/hexagons/
-- e, se, sw, w, nw, and ne.
--
--            (q, r, s)
--                .
--    s+1, r-1   / \
--              /q  \ q+1, r-1
--             /     \
--            |       |
--   s+1, q-1 |      r|  q+1, s-1
--            |       |
--             \     /
--              \s  /  r+1, s-1
--    r+1, q-1   \ /
--                .

main = do
  putStrLn "start"
  args <- getArgs
  content <- readFile (head args)
  let input = fromJust $ parseMaybe dirParser content
  let flips = countFlips input
  let blacks = M.keysSet $ M.filter odd flips
  print $ S.size blacks
  print $ S.size $ iterate step blacks !! 100

--            q    r    s
type Coord = (Int, Int, Int)

type Offset = Coord

step :: S.Set Coord -> S.Set Coord
step blacks =
  let toConsider = getCandidates blacks
   in S.unions $ S.map (applyRules blacks) toConsider

-- Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
-- Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
applyRules :: S.Set Coord -> Coord -> S.Set Coord
applyRules blacks c =
  let n = neighbors c
      bn = S.size $ blacks `S.intersection` n
      isBlack = S.member c blacks
      rule True 1 = S.singleton c
      rule True 2 = S.singleton c
      rule True _ = S.empty
      rule False 2 = S.singleton c
      rule False _ = S.empty
   in rule isBlack bn

getCandidates :: S.Set Coord -> S.Set Coord
getCandidates blacks = S.unions $ S.map neighbors blacks

neighbors :: Coord -> S.Set Coord
neighbors c = S.fromList $ map ((c ^+^) . toOffset) [E, SE, SW, W, NW, NE]

countFlips :: [[Dir]] -> M.Map Coord Int
countFlips dirs = go M.empty $ map identify dirs
  where
    go seen [] = seen
    go seen (x : xs) = go (M.insertWith (+) x 1 seen) xs

identify :: [Dir] -> Coord
identify dirs = foldl (^+^) (0, 0, 0) $ map toOffset dirs

toOffset :: Dir -> Coord
toOffset E = (1, 0, -1)
toOffset SE = (0, 1, -1)
toOffset SW = (-1, 1, 0)
toOffset W = (-1, 0, 1)
toOffset NW = (0, -1, 1)
toOffset NE = (1, -1, 0)

type Parser = Parsec Void String

sc = L.space (skipSome (char ' ')) A.empty A.empty

symbol = L.symbol sc

fromString :: String -> Dir
fromString "e" = E
fromString "se" = SE
fromString "sw" = SW
fromString "w" = W
fromString "nw" = NW
fromString "ne" = NE
fromString x = error $ "fromString ooop " ++ x

dirParser :: Parser [[Dir]]
dirParser = do
  x <- some (dirLineParser <* try (symbol "\n"))
  eof
  return x

dirLineParser :: Parser [Dir]
dirLineParser = some singleDirParser

singleDirParser :: Parser Dir
singleDirParser = do
  let directions = ["e", "se", "sw", "w", "nw", "ne"]
  match <- choice $ map (try . symbol) directions
  return $ fromString match
