module Main where

import Text.ParserCombinators.Parsec
import Parsing

data Game = Game Int [Colors] | Error
  deriving (Read, Show, Eq)

data Colors = C Color | Cs Color Colors
  deriving (Read, Show, Eq)

data Color = Blue Int | Red Int | Green Int
  deriving (Read, Show, Eq)

toGame :: String -> Game
toGame s = case parseGame s of
             Right g -> g
             Left _ -> Error

parseGame :: String -> Either ParseError Game
parseGame = parse game ""

game :: Parser Game
game = do spaces
          symbol "Game"
          n <- digits
          pChar ':'
          cs <- colorSet
          return $ Game n cs

colorSet :: Parser [Colors]
colorSet =   try (do c <- colors
                     pChar ';'
                     cs <- colorSet
                     return $ (c:cs))
         <|> do c <- colors
                return [c]

colors :: Parser Colors
colors =   try (do n <- digits
                   c <- color n
                   pChar ','
                   cs <- colors
                   return $ Cs c cs)
       <|> do n <- digits
              c <- color n
              return $ C c

color :: Int -> Parser Color
color n =   do symbol "blue"; return $ Blue n
        <|> do symbol "green"; return $ Green n
        <|> do symbol "red"; return $ Red n

possible :: Game -> Int
possible Error = 0
possible (Game _ []) = 0
possible (Game n cs) = if foldr (&&) True (map possibleColorSet cs) then n else 0

possibleColorSet :: Colors -> Bool
possibleColorSet (C c) = possibleColor c
possibleColorSet (Cs c cs) = possibleColor c && possibleColorSet cs

possibleColor :: Color -> Bool
possibleColor (Blue n) = n <= 14
possibleColor (Red n) = n <= 12
possibleColor (Green n) = n <= 13

fewestCombine :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fewestCombine (x0, x1, x2) (y0, y1, y2) = (max x0 y0, max x1 y1, max x2 y2)

fewestPower :: (Int, Int, Int) -> Int
fewestPower (x0, x1, x2) = x0 * x1 * x2

fewest :: Game -> (Int, Int, Int)
fewest Error = (0,0,0)
fewest (Game _ []) = (0,0,0)
fewest (Game _ cs) = foldr (fewestCombine) (0,0,0) (map fewestColorSet cs)

fewestColorSet :: Colors -> (Int, Int, Int)
fewestColorSet (C c) = fewestColor c
fewestColorSet (Cs c cs) = fewestCombine (fewestColor c) $ fewestColorSet cs

fewestColor :: Color -> (Int, Int, Int)
fewestColor (Blue n) = (0,0,n)
fewestColor (Red n) = (n,0,0)
fewestColor (Green n) = (0,n,0)

main = do input <- getLines
          print $ foldr (+) 0 $ map possible $ map toGame input
          print $ foldr (+) 0 $ map fewestPower $ map fewest $ map toGame input
