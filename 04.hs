module Main where

import Text.ParserCombinators.Parsec
import Common.Parsing

data Scratchcard = Card Int [Int] [Int]
  deriving (Read, Show, Eq)

cards :: [String] -> [Scratchcard]
cards [] = []
cards (c:cs) = case runParser (card "") () "" c of
                     Right c' -> c':cs'
                     Left _ -> cs'
  where cs' = cards cs

card :: String -> Parser Scratchcard
card card = do spaces; symbol "Card"; n <- digits
               pChar ':'; ns <- many1 digits
               pChar '|'; ns' <- many1 digits
               return $ Card n ns ns'

winNum :: Scratchcard -> Int
winNum (Card _ wins nums) = foldr (+) 0 $ map (winNum' wins) nums

winNum' :: [Int] -> Int -> Int
winNum' [] _ = 0
winNum' (w:ws) n
  | w == n = 1
  | otherwise = winNum' ws n

doubling :: Int -> Int
doubling n
  | n == 0 = 0
  | n == 1 = 1
  | n <  0 = -1
  | n >  1 = 2 * (doubling $ n-1)

winMore :: [Int] -> [Int] -> [Int]
winMore [] _ = []
winMore _ [] = []
winMore (c:cs) (w:ws) = c:(winMore (propagateCards c w cs) ws)

propagateCards :: Int -> Int -> [Int] -> [Int]
propagateCards _ _ [] = []
propagateCards n i (c:cs)
  | 0 < i = (c+n):(propagateCards n (i-1) cs)
  | otherwise = (c:cs)

main = do input <- getLines
          print $ foldr (+) 0 $ map doubling $ map winNum $ cards input
          print $ foldr (+) 0 $ winMore (take (length input) (repeat 1)) $ map winNum $ cards input
