import qualified Control.Applicative as A
import Data.Void (Void)
import Text.Megaparsec
    ( parseTest, choice, skipSome, Parsec, MonadParsec(eof, try) )
import Text.Megaparsec.Char ( char )
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.ParserCombinators.ReadP as R

main :: IO ()
main = do
  let x = "aaaaa"
  parseTest rstart x
  print $ R.readP_to_S sstart x

type Parser = Parsec Void String

r0 ::  Parser String
r0  = symbol "a"
r8 ::  Parser String
r8  = choice [try r0, try ((++) <$> r0 <*> r8)]
rstart :: Parser String
rstart = r8 <* eof

s0 ::  R.ReadP  String
s0  = (: []) <$> R.char 'a'
s8 ::  R.ReadP  String
s8  =  s0 R.+++ ( (++) <$> s0 <*> s8)
sstart :: R.ReadP  String
sstart = s8 <* R.eof


sc = L.space (skipSome (char ' ')) A.empty A.empty

symbol = L.symbol sc
