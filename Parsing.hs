module Parsing where

import Text.ParserCombinators.Parsec

-- Reads lines from stdin
getLines :: IO [String]
getLines = do
  x <- getLine
  if x == ""
  then return []
  else do
    xs <- getLines
    return (x:xs)

-- Parser combinators
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

symbol :: String -> Parser String
symbol s = lexeme $ string s

digits :: Parser Int
digits = lexeme $ do ds <- many1 digit; return $ read ds

pChar :: Char -> Parser Char
pChar = lexeme . char
