import qualified Control.Applicative as A
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either (fromRight)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Void (Void)
import Maybes (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)
import Prelude hiding (either)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let d = fromJust $ parseMaybe parseInput content
  let ex = explode d
  let ingredientsByAllergen = intersections ex
  let possibleIngredients = foldr1 S.union (M.elems ingredientsByAllergen)
  let allIngredients = concatMap snd d
  let inertIngredients = [ing | ing <- allIngredients, not $ S.member ing possibleIngredients]
  print "Part 1"
  putStrLn $ unwords ["Inert ingredients appear", show $ length inertIngredients, "times"]

  let reducedbyAllergen = M.map (\ing -> ing `S.difference` S.fromList inertIngredients) ingredientsByAllergen
  print "Part 2"
  putStrLn $ intercalate "," $ map snd $ sortBy (\a b -> fst a `compare` fst b) $ reduce reducedbyAllergen

reduce :: M.Map String (S.Set String) -> [(String, String)]
reduce byAllergen
  | M.empty == byAllergen = []
  | otherwise =
    let singletons = M.filter (\x -> S.size x == 1) byAllergen
        singletonList = map (\(a, b) -> (a, head $ S.toList b)) $ M.toList singletons
        singletonIngredients = foldr1 S.union (M.elems singletons)
        rest = M.filter (\x -> S.size x /= 1) byAllergen
        removed = M.map (\ing -> ing `S.difference` singletonIngredients) rest
     in singletonList ++ reduce removed

intersections :: [(String, [String])] -> M.Map String (S.Set String)
intersections = foldr (\a b -> M.insertWith S.intersection (fst a) (S.fromList (snd a)) b) M.empty

explode :: [([String], [String])] -> [(String, [String])]
explode d = [(allergen, ingredients) | (allergens, ingredients) <- d, allergen <- allergens]

type Parser = Parsec Void String

sc = L.space (skipSome (char ' ')) A.empty A.empty

lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol = L.symbol sc

parseInput :: Parser [([String], [String])]
parseInput = do
  exs <- sepEndBy1 parseLine newline
  _ <- eof
  return exs

-- sqjhc mxmxvkd sbzzf (contains dairy, fish)
parseLine :: Parser ([String], [String])
parseLine = do
  ingredients <- some (word <* char ' ')
  _ <- symbol "(contains "
  allergens <- sepBy word (symbol ",")
  _ <- symbol ")"
  return (allergens, ingredients)

word :: Parser String
word = some lowerChar
