
import Text.Megaparsec

import Text.Megaparsec.Char ( char )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import qualified Control.Applicative as A
import Text.Megaparsec.Debug
import Data.Either (fromRight)
import TcRnMonad (TcGblEnv(tcg_dependent_files))
import Control.Monad (guard)

type Board = [[Char]]
type Borders = ([Char], [Char], [Char], [Char])
type Tile = (Integer, Board)
type Parser = Parsec Void String

main :: IO ()
main = do
  content <- readFile "input.txt"
  let boards = fromRight [(0,["oops"])]  (parse parseInput "foo" content)
  let first = head boards

  let corners = filter (\x -> snd x == 2) $ allMatches boards
  print corners
  print $ product $ map fst corners

  let sides = filter (\x -> snd x == 3) $ allMatches boards
  let inner = filter (\x -> snd x == 4) $ allMatches boards
  print $ length corners
  print $ length sides
  print $ length inner


allMatches :: [Tile] -> [(Integer, Int)]
allMatches tiles = map (\x -> (fst x, length $ getMatches x tiles)) tiles

getMatches :: Tile -> [Tile] -> [Integer]
getMatches tile tiles = do
  t <- tiles
  guard $ fst t /= fst tile
  let tileSides = getSides (snd tile)
  let testSides = getSides (snd t)
  guard $ haveMatch tileSides testSides

  return $ fst t

haveMatch :: Borders -> Borders -> Bool
haveMatch (a,b,c,d) (e,f,g,h) = or [ l == r || l == reverse r | l <- [a,b,c,d], r <- [e,f,g,h]]

getSides :: Board -> Borders
getSides board = let
  size = length board
  a = head board
  b = [(board !! i ) !! (size - 1) | i <- [0..size-1]]
  c =  board !! (size - 1)
  d = [head (board !! i ) | i <- [0..size-1]]
  in (a,b,c,d)


parseInput :: Parser [Tile]
parseInput = many parseSingle <* eof

parseSingle :: Parser Tile
parseSingle = do
  i <-  symbol "Tile " *> integer <* symbol ":\n"
  b <- board
  symbol "\n"
  return (i, b)


board :: Parser Board
board =   some line


line :: Parser String
line =  some (oneOf ".#") <* char '\n'

integer :: Parser Integer
integer =  lexeme L.decimal

sc = L.space (skipSome (char ' ')) A.empty A.empty

symbol = L.symbol sc
lexeme = L.lexeme sc
