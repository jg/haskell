module Reddit.Config where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Data.Char
import Data.Maybe
import Text.Parsec.Error
import qualified Data.Map as Map
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

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
  char ':'
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

readConfigFile :: SourceName -> IO (Either ParseError Config)
readConfigFile name =
  parseFromFile file name >>=
  return . fmap (foldr (uncurry Map.insert) Map.empty . reverse) 

lookupVar :: Config -> String -> String
lookupVar config name = fromJust (Map.lookup name config)

connectionUrl :: ReaderT Config IO B.ByteString
connectionUrl = do
  config <- ask
  return $ BC.pack (url config)
  where
    names = ["host", "port", "dbname", "password"]
    values config = fmap (lookupVar config) names
    url config = unwords $
      zipWith (\a b -> a ++ "=" ++ b) names (values config)

readConfig = readConfigFile "config.cfg"
