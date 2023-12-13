module Main where

import Parsing
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

parseHandsRun :: String -> ([Int],Int)
parseHandsRun s = case runParser parseHands () "" s of
                    Right hand -> hand
                    Left err -> error $ show err

parseHands :: Parser ([Int], Int)
parseHands = do spaces
                c1 <- parseCard
                c2 <- parseCard
                c3 <- parseCard
                c4 <- parseCard
                c5 <- parseCard
                bid <- digits
                return (c1:c2:c3:c4:c5:[],bid)

parseCard :: Parser Int
parseCard =   (pChar '2' >> return 2)
          <|> (pChar '3' >> return 3)
          <|> (pChar '4' >> return 4)
          <|> (pChar '5' >> return 5)
          <|> (pChar '6' >> return 6)
          <|> (pChar '7' >> return 7)
          <|> (pChar '8' >> return 8)
          <|> (pChar '9' >> return 9)
          <|> (pChar 'T' >> return 10)
          <|> (pChar 'J' >> return 11)
          <|> (pChar 'Q' >> return 12)
          <|> (pChar 'K' >> return 13)
          <|> (pChar 'A' >> return 14)

findRank :: [Int] -> Int
findRank [c1,c2,c3,c4,c5]
  | c1 == c2 && c2 == c3 && c3 == c4 && c4 == c5
    = 7 -- five of a kind
  | c1 == c2 && c2 == c3 && c3 == c4 ||
    c1 == c2 && c2 == c3 && c3 == c5 ||
    c1 == c2 && c2 == c4 && c4 == c5 ||
    c1 == c3 && c3 == c4 && c4 == c5 ||
    c2 == c3 && c3 == c4 && c4 == c5
    = 6 -- four of a kind
  | c1 == c2 && c2 == c3 && c4 == c5 ||
    c1 == c2 && c2 == c4 && c3 == c5 ||
    c1 == c2 && c2 == c5 && c3 == c4 ||
    c1 == c3 && c3 == c4 && c2 == c5 ||
    c1 == c3 && c3 == c5 && c2 == c4 ||
    c1 == c4 && c4 == c5 && c2 == c3 ||
    c1 == c2 && c3 == c4 && c4 == c5 ||
    c1 == c3 && c2 == c4 && c4 == c5 ||
    c1 == c4 && c2 == c3 && c3 == c5 ||
    c1 == c5 && c2 == c3 && c3 == c4
    = 5 -- full house
  | c1 == c2 && c2 == c3 ||
    c1 == c2 && c2 == c4 ||
    c1 == c2 && c2 == c5 ||
    c1 == c3 && c3 == c4 ||
    c1 == c3 && c3 == c5 ||
    c1 == c4 && c4 == c5 ||
    c2 == c3 && c3 == c4 ||
    c2 == c3 && c3 == c5 ||
    c2 == c4 && c4 == c5 ||
    c3 == c4 && c4 == c5
    = 4 -- three of a kind
  | c1 == c2 && c3 == c4 ||
    c1 == c2 && c3 == c5 ||
    c1 == c2 && c4 == c5 ||
    c1 == c3 && c2 == c4 ||
    c1 == c3 && c2 == c5 ||
    c1 == c3 && c4 == c5 ||
    c1 == c4 && c2 == c3 ||
    c1 == c4 && c2 == c5 ||
    c1 == c4 && c3 == c5 ||
    c1 == c5 && c2 == c3 ||
    c1 == c5 && c2 == c4 ||
    c1 == c5 && c3 == c4 ||
    c2 == c3 && c4 == c5 ||
    c2 == c4 && c3 == c5 ||
    c2 == c5 && c3 == c4
    = 3 -- two pairs
  | c1 == c2 || c1 == c3 || c1 == c4 || c1 == c5 ||
    c2 == c3 || c2 == c4 || c2 == c5 ||
    c3 == c4 || c3 == c5 ||
    c4 == c5
    = 2 -- one pair
  | otherwise
    = 1 -- hight card
findRank _ = -1

compareHands :: ([Int], (Int, Int)) -> ([Int], (Int, Int)) -> Ordering
compareHands ([c1,c2,c3,c4,c5], (r, _)) ([c1',c2',c3',c4',c5'], (r', _)) =
  if r /= r' then compare r r' else
  if c1 /= c1' then compare c1 c1' else
  if c2 /= c2' then compare c2 c2' else
  if c3 /= c3' then compare c3 c3' else
  if c4 /= c4' then compare c4 c4' else
                    compare c5 c5'
compareHands _ _ = undefined

main :: IO ()
main = do input <- getLines
          print $ foldr (\(o, (_, (_, b))) acc -> (o * b) + acc) 0 $
            zip [1..] $ sortBy compareHands $
            map ((\(h, b) -> (h, (findRank h, b))) . parseHandsRun) input
