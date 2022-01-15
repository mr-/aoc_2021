import qualified Control.Applicative as A
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either (fromRight)
import Data.Map as M hiding (filter, map)
import Data.Maybe (isJust)
import Data.Void (Void)
import Maybes (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (either)

main :: IO ()
main = do
  content <- readFile "sample_2_altered.txt"


  let input = fromJust $ parseMaybe parseInput content
  let rulesChecker = toChecker (fst input)
  let samples = snd input

  let valids = filter (\x -> isJust (parseMaybe rulesChecker x)) samples
  print valids
  print $ length valids

  print ""

type Parser = Parsec Void String

type Rules = M.Map Int Rule

data Rule
  = RLiteral Char
  | REither Rule Rule
  | RFollowed [Int]
  deriving (Show)

toChecker :: Rules -> Parser String
toChecker rules = toChecker' rules (fromJust $ M.lookup 0 rules)

toChecker' :: Rules -> Rule -> Parser String
toChecker' rules (RLiteral c) = symbol [c]
toChecker' rules (REither l r) = choice [try (toChecker' rules l), try (toChecker' rules r)]
toChecker' rules (RFollowed rs) = head <$> mapM (toChecker' rules) [fromJust (M.lookup r rules) | r <- rs]

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
