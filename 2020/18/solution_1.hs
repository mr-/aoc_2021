import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, many, parse, (<?>), (<|>))
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char (numberChar, space1, spaceChar)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  content <- readFile "input.txt"
  let results = parseAll (lines content)

  print $ sum results

parseAll :: [String] -> [Int]
parseAll = map parseLine

parseLine :: String -> Int
parseLine x = fromRight 0 $ parse pExpr "" x

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    space1
    space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

table =
  [ [ binary "+" (+),
      binary "*" (*)
    ]
  ]

binary name f = InfixL (f <$ symbol name)

pInteger :: Parser Int
pInteger = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Int
pTerm =
  choice
    [ parens pExpr,
      pInteger
    ]

pExpr :: Parser Int
pExpr = makeExprParser pTerm table
