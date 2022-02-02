import qualified Control.Applicative as A
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Tree
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    choice,
    many,
    manyTill,
    optional,
    parse,
    parseMaybe,
    parseTest,
    sepBy1,
    skipSome,
    some,
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L

main = do
  args <- getArgs
  content <- readFile (head args)

  let orbits = fromJust $ parseMaybe parseFile content
  let tree = toTree orbits
  -- putStr $ drawTree tree
  print "Solution 1"
  print $ depthSum tree
  let paths = map snd $ findPaths tree "YOU" "SAN"
  let (a, b) = dropCommonPrefix (head paths) (paths !! 1)
  print "Solution 2"
  print $ length b -1 + length a -1

dropCommonPrefix :: [String] -> [String] -> ([String], [String])
dropCommonPrefix a@(x : xs) b@(y : ys)
  | y == x = dropCommonPrefix xs ys
  | otherwise = (a, b)
dropCommonPrefix a b = error "dropCommonPrefix WTF"

findPaths :: Tree String -> String -> String -> [(String, [String])]
findPaths tree x y =
  let ps = paths tree
      res = filter (\(a, as) -> a == x || a == y) ps
   in res

paths :: Tree a -> [(a, [a])]
paths = foldTree algebra
  where
    algebra a as = (a, [a]) : map (\(i, is) -> (i, a : is)) (concat as)

type Orbit = (String, String)

depthSum :: Tree String -> Int
depthSum tree =
  let ls = levels tree
      levelSum i (x : xs) = length x * i + levelSum (i + 1) xs
      levelSum i [] = 0
   in levelSum 0 ls

toTree :: [Orbit] -> Tree String
toTree orbits =
  let m = toTreeMap orbits
      buildNode x = if x `M.member` m then (x, m M.! x) else (x, [])
      res = unfoldTree buildNode "COM"
   in res

toTreeMap :: [Orbit] -> Map String [String]
toTreeMap orbits =
  let listed = map (\(x, y) -> (x, [y])) orbits
      f (k, v) m = M.insertWith (++) k v m
      res = foldr f M.empty listed
   in res

type Parser = Parsec Void String

sc = L.space (skipSome (char ' ')) A.empty A.empty

symbol = L.symbol sc

parseFile :: Parser [Orbit]
parseFile = do
  x <- some parseLine
  eof
  return x

parseLine :: Parser Orbit
parseLine = do
  x <- manyTill alphaNumChar (symbol ")")
  y <- manyTill alphaNumChar (symbol "\n")
  return (x, y)
