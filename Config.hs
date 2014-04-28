import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

type Config = Map.Map String String

ident :: Parser String

ident = do
  c <- letter
  cs <- many (letter <|> digit <|> char '_')
  return (c:cs)
  <?> "identifier"

comment :: Parser ()

comment = do
  char '#'
  skipMany (noneOf ("\r\n"))
  <?> "comment"

eol :: Parser ()

eol = do
  oneOf "\n\r"
  return ()
  <?> "end of line"

item :: Parser (String, String)

item = do
  key <- ident
  skipMany space
  char '='
  skipMany space
  value <- manyTill anyChar (try eol <|> try comment <|> eof)
  return (key, rstrip value)
  where rstrip = reverse . dropWhile isSpace . reverse
            

line :: Parser (Maybe (String, String))

line = do
  skipMany space
  try (comment >> return Nothing) <|> (item >>= return . Just)


file :: Parser [(String, String)]

file = do
  lines <- many line
  return (catMaybes lines) 

readConfig :: SourceName -> IO (Either ParseError Config)

readConfig name =
  parseFromFile file name >>=
  return . fmap (foldr (uncurry Map.insert) Map.empty . reverse) 
