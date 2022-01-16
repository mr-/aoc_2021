import qualified Control.Applicative as A
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either (fromRight)
import Data.Map as M hiding (null, filter, map)
import Data.Maybe (isJust)
import Data.Void (Void)
import Maybes (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (either)
import Text.Megaparsec.Debug (dbg)
import qualified Text.ParserCombinators.ReadP as R

main :: IO ()
main = do
  content <- readFile "input_2.txt"

  let input = fromJust $ parseMaybe parseInput content
  let rulesChecker = toChecker (fst input)
  let samples = snd input

  let valids = filter (checkChecker rulesChecker) samples
  print $ length valids

type Parser = Parsec Void String
type Rules = M.Map Int Rule

data Rule
  = RLiteral Char
  | REither Rule Rule
  | RFollowed [Int]
  deriving (Show)

checkChecker :: R.ReadP Char -> String -> Bool
checkChecker checker str = let
  matches =  R.readP_to_S checker str
  filtered = filter (\(a,xs) -> null xs) matches
  in
    not (null matches) && not (null filtered)

toChecker :: Rules -> R.ReadP Char
toChecker rules = toChecker' rules (rules ! 0)

toChecker' :: Rules -> Rule -> R.ReadP Char
toChecker' rules (RLiteral c) =  R.char c
toChecker' rules (REither l r) =   toChecker' rules l R.+++  toChecker' rules r
toChecker' rules (RFollowed rs) =  foldr1 (*>) (map (\r -> toChecker' rules (rules ! r) ) rs)

sc = L.space (skipSome (char ' ')) A.empty A.empty

lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol = L.symbol sc

parseInput :: Parser (Rules, [String])
parseInput = do
  r <- parseRules
  _ <- newline
  exs <- sepEndBy1 (some letterChar) newline
  return (r, exs)

type FullRule = (Int, Rule)

parseRules :: Parser Rules
parseRules = M.fromList <$> sepEndBy1 parseRule newline

parseRule :: Parser FullRule
parseRule = (,) <$> ruleHead <*> choice [try either, try followed, try literal]

ruleHead :: Parser Int
ruleHead = integer <* symbol ":"

either :: Parser Rule
either = REither <$> followed <* symbol "|" <*> followed

followed :: Parser Rule
followed = do
  digits <- some integer
  return $ RFollowed digits

literal :: Parser Rule
literal = RLiteral <$> between (symbol "\"") (symbol "\"") letterChar

rules :: Parser String
rules = undefined

examples :: Parser String
examples = undefined
