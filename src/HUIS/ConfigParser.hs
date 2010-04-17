module HUIS.ConfigParser where
import Data.Char
import Control.Monad
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Maybe

type Config = Map.Map String String


ident :: Parser String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           return (c:cs)
      <?> "identifier"


comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")
             <?> "comment"

eol :: Parser ()
eol = do oneOf "\n\r"
         return ()
    <?> "end of line"



item :: Parser (String, String)
item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, rstrip value)
    where rstrip = reverse . dropWhile isSpace . reverse  --remove leading whitespace...


line :: Parser (Maybe (String, String))
line = do skipMany space
          try (comment >> return Nothing) <|> fmap Just item


file :: Parser [(String, String)]
file = do lines <- many line
          return (catMaybes lines)
          
listToMap:: [(String, String)] -> Config
listToMap = foldr (uncurry Map.insert) Map.empty 
           
readConfig :: SourceName -> IO Config
readConfig name = do
  result <- parseFromFile file name
  return $ case result of 
    Left err -> Map.empty
    Right xs -> listToMap (reverse xs)
