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

getLinesStop :: IO [String]
getLinesStop = do
  x <- getLine
  if x == "stop"
  then return []
  else do
    xs <- getLinesStop
    return (x:xs)

collapseInput :: [String] -> String
collapseInput [] = ""
collapseInput (l:ls) = l ++ "\n" ++ (collapseInput ls)

-- Parser combinators
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

symbol :: String -> Parser String
symbol s = lexeme $ string s

digits :: Parser Int
digits = lexeme $ read <$> many1 digit

pChar :: Char -> Parser Char
pChar = lexeme . char
