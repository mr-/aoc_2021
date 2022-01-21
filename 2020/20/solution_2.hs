import qualified Control.Applicative as A
import Control.Monad (guard)
import Control.Monad.State
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (fromRight)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Board = Map Position Char

type Borders = (String, String, String, String)

data Tile = Tile {tileId :: Integer, tileBoard :: Board} deriving (Show)

type Tiling = [(Position, Tile)]

type Parser = Parsec Void String

data Position = Position {x :: Int, y :: Int} deriving (Eq, Ord)

instance Show Position where
  show (Position x y) = show (x, y)

data Monster = Monster {monsterW :: Int, monsterH :: Int, monsterPs :: S.Set Position}

main :: IO ()
main = do
  content <- readFile "input.txt"
  let tiles = fromRight [Tile 0 M.empty] (parse parseInput "foo" content)
  monster <- getMonster

  let start = head tiles

  let m = allMatches tiles
  let origTiling = getTiling m (Position 0 0, start)
  let tiling = normalize $ map (second (normalizeTile . cutBorder)) origTiling
  let board = toBoard tiling
  let (b, mp) = findMonsterPoints monster board

  print $ S.size (getPoints b) - S.size mp

findMonsterPoints :: Monster -> Board -> (Board, S.Set Position)
findMonsterPoints monster board =
  let all = allBoards board
      found = filter (\(_, m) -> not $ S.null m) $ map (\b -> (b, findMonsters monster b)) all
   in head found

getPoints :: Board -> S.Set Position
getPoints board = M.keysSet $ M.filter (== '#') board

allBoards :: Board -> [Board]
allBoards board =
  let rotated = [brotate r board | r <- [0 .. 3]]
      flippedAndRotated = [brotate r $ bflip 0 board | r <- [0 .. 3]]
   in rotated ++ flippedAndRotated

findMonsters :: Monster -> Board -> S.Set Position
findMonsters (Monster _ _ mps) board =
  let boardKeys = M.keysSet $ M.filter (== '#') board
      ((minX, maxX), (minY, maxY)) = getDimensions board
      positions = [Position xs ys | xs <- [minX .. maxX], ys <- [minY .. maxY]]
      translatedMonster t = translate t mps
      found t = translatedMonster t `S.intersection` boardKeys
      success t = S.size (found t) == S.size mps
   in S.fromList
        [ p
          | t <- positions,
            success t,
            p <- S.toList (translatedMonster t)
        ]

translate :: Position -> S.Set Position -> S.Set Position
translate p = S.map (\(Position sx sy) -> Position (x p + sx) (y p + sy))

getMonster :: IO Monster
getMonster = do
  content <- readFile "monster.txt"
  return $ parseMonster content

parseMonster :: String -> Monster
parseMonster s =
  let l = lines s
      width = length (head l)
      height = length l

      ps = S.fromList [Position (fst char) (fst line) | line <- zip [0 :: Int ..] l, char <- zip [0 :: Int ..] (snd line), snd char == '#']
   in Monster width height ps

normalizeTile :: Tile -> Tile
normalizeTile t = Tile (tileId t) (M.fromList $ normalize $ M.toList $ tileBoard t)

--      0
--     ----
--  3 |    | 1
--     ----
--      2

type Neighbors = Map Integer [Tile]

getTiling :: Neighbors -> (Position, Tile) -> Tiling
getTiling allNeighbours initial =
  evalState (go initial) S.empty
  where
    go :: (Position, Tile) -> State (S.Set Integer) Tiling
    go (position, tile) = do
      let neighbours = allNeighbours M.! tileId tile
      seenSoFar <- get
      let toAdd = filter (\x -> tileId x `S.notMember` seenSoFar) neighbours
      let relativeGluedNeighbours = map (glue tile) toAdd
      let gluedIds = S.fromList $ map (\(_, t) -> tileId t) relativeGluedNeighbours
      _ <- modify (`S.union` gluedIds)
      concat
        <$> mapM
          ( \(rPos, rotatedTile) ->
              do
                let newPos = Position (x position + x rPos) (y position + y rPos)
                tilingSoFar <- go (newPos, rotatedTile)
                return $ (newPos, rotatedTile) : tilingSoFar
          )
          relativeGluedNeighbours

glue :: Tile -> Tile -> (Position, Tile)
glue oTile nTile =
  let ((oPos, _), (nPos, _)) = head $ getMatchingSides oTile nTile
      relPos 0 = Position 0 (-1)
      relPos 1 = Position 1 0
      relPos 2 = Position 0 1
      relPos 3 = Position (-1) 0
      relPos x = error "well.."
      -- gluePos is the other side..
      gluePos = (oPos - 2) `mod` 4
      -- have to rotate so that nPos goes to gluePos
      rotations = (gluePos - nPos) `mod` 4
      rotated = brotate rotations (tileBoard nTile)
      success = nth oPos (tileBoard oTile) == nth gluePos rotated
      newBoard = if success then rotated else bflip gluePos rotated
   in (relPos oPos, Tile (tileId nTile) newBoard)

nth :: Int -> Board -> String
nth n board =
  let (o0, o1, o2, o3) = getSides board
      go 0 = o0
      go 1 = o1
      go 2 = o2
      go 3 = o3
      go _ = error "nth uoho"
   in go n

brotate :: Int -> Board -> Board
brotate rotCount board =
  let entries = M.toList board
      rot 3 (Position x y) = Position y (- x)
      rot 2 (Position x y) = Position (- x) (- y)
      rot 1 (Position x y) = Position (- y) x
      rot 0 (Position x y) = Position x y
      rot x _ = error ("Uoho rot" ++ show x)
      newEntries = map (first (rot rotCount)) entries
   in M.fromList newEntries

bflip :: Int -> Board -> Board
bflip flipEdge board =
  let entries = M.toList board
      ((minX, maxX), (minY, maxY)) = getDimensions board
      bflip 3 (Position x y) = Position x (maxY - (y - minY))
      bflip 1 (Position x y) = Position x (maxY - (y - minY))
      bflip 0 (Position x y) = Position (maxX - (x - minX)) y
      bflip 2 (Position x y) = Position (maxX - (x - minX)) y
      bflip x _ = error $ "Uoho bflip" ++ show x
      newEntries = map (first (bflip flipEdge)) entries
   in M.fromList newEntries

cutBorder :: Tile -> Tile
cutBorder tile =
  let ((minX, maxX), (minY, maxY)) = getDimensions (tileBoard tile)
      board = tileBoard tile
      filtered = M.filterWithKey (\(Position x y) _ -> x /= minX && x /= maxX && y /= maxY && y /= minY) board
   in Tile (tileId tile) filtered

toBoard :: Tiling -> Board
toBoard tiling =
  let normalized = normalize tiling
      nTiling = M.fromList normalized
      maxX = maximum [x p | (p, _) <- normalized]
      maxY = maximum [y p | (p, _) <- normalized]
      ((_, boardMaxX), (_, boardMaxY)) = getDimensions (tileBoard (snd (head tiling)))
      (bX, bY) = (boardMaxX + 1, boardMaxY + 1)
      xs = [0 .. (maxX + 1) * bX -1]
      ys = [0 .. (maxY + 1) * bY -1]
      tilePos x y = Position (x `div` bX) (y `div` bY)
      inTilePos x y = Position (x `mod` bX) (y `mod` bY)
      entry x y = tileBoard (nTiling M.! tilePos x y) M.! inTilePos x y
   in M.fromList [(Position x y, entry x y) | x <- xs, y <- ys]

normalize :: [(Position, a)] -> [(Position, a)]
normalize tiling =
  let xs = [x (fst p) | p <- tiling]
      ys = [y (fst p) | p <- tiling]
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)
      tr (Position x y) = Position (x - minX) (y - minY)
   in map (first tr) tiling

ppMTiling :: Map Position Tile -> String
ppMTiling = ppTiling . M.toList

ppTiling :: Tiling -> String
ppTiling tiling =
  let mTiling = M.fromList tiling
      xs = [x (fst p) | p <- tiling]
      ys = [y (fst p) | p <- tiling]
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)
      c x y = if M.member (Position x y) mTiling then '.' else ' '
   in unlines $ [[c x y | x <- [minX .. maxX]] | y <- [minY .. maxY]]

ppTile :: Tile -> String
ppTile = ppBoard . tileBoard

ppBoard :: Board -> String
ppBoard board =
  let entries = M.toList board
      xs = [x (fst p) | p <- entries]
      ys = [y (fst p) | p <- entries]
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)
   in unlines $ [[board M.! Position x y | x <- [minX .. maxX]] | y <- [minY .. maxY]]

allMatches :: [Tile] -> Neighbors
allMatches tiles = M.fromList $ map (\x -> (tileId x, getMatches x tiles)) tiles

getMatches :: Tile -> [Tile] -> [Tile]
getMatches tile tiles = do
  t <- tiles
  guard $ tileId t /= tileId tile
  guard $ haveMatch tile t

  return t

haveMatch :: Tile -> Tile -> Bool
haveMatch oTile nTile = not . null $ getMatchingSides oTile nTile

getMatchingSides :: Tile -> Tile -> [((Int, String), (Int, String))]
getMatchingSides oTile nTile =
  let (o1, o2, o3, o4) = getSides $ tileBoard oTile
      (n1, n2, n3, n4) = getSides $ tileBoard nTile
   in [(l, r) | l <- zip [0 ..] [o1, o2, o3, o4], r <- zip [0 ..] [n1, n2, n3, n4], snd l == snd r || snd l == reverse (snd r)]

getDimensions :: Board -> ((Int, Int), (Int, Int))
getDimensions board =
  let entries = M.toList board
      xs = [x (fst p) | p <- entries]
      ys = [y (fst p) | p <- entries]
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)
   in ((minX, maxX), (minY, maxY))

getSides :: Board -> Borders
getSides board =
  let entries = M.toList board
      ((minX, maxX), (minY, maxY)) = getDimensions board
      t xs = map snd $ sortBy (\a b -> compare (fst a) (fst b)) xs

      a = [p | p <- entries, y (fst p) == minY]
      b = [p | p <- entries, x (fst p) == maxX]
      c = [p | p <- entries, y (fst p) == maxY]
      d = [p | p <- entries, x (fst p) == minX]
   in (t a, t b, t c, t d)

parseInput :: Parser [Tile]
parseInput = many parseSingle <* eof

parseSingle :: Parser Tile
parseSingle = do
  i <- symbol "Tile " *> integer <* symbol ":\n"
  b <- parseBoard
  symbol "\n"
  return $ Tile i b

parseBoard :: Parser Board
parseBoard = toCoords <$> some line

toCoords :: [String] -> Board
toCoords lines = M.fromList $ do
  line <- zip [0 :: Int ..] lines
  char <- zip [0 :: Int ..] (snd line)

  return (Position (fst char) (fst line), snd char)

line :: Parser String
line = some (oneOf ".#") <* char '\n'

integer :: Parser Integer
integer = lexeme L.decimal

sc = L.space (skipSome (char ' ')) A.empty A.empty

symbol = L.symbol sc

lexeme = L.lexeme sc
