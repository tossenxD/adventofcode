module Main where

import Text.ParserCombinators.Parsec
import Parsing
import Data.Char
import Control.Parallel

parseRecords :: Parser [(Int,Int)]
parseRecords = do spaces; symbol "Time:"; ts <- many1 digits
                  symbol "Distance:"; ds <- many1 digits
                  return $ zip ts ds

parseRecord :: [String] -> (Int, Int)
parseRecord ['T':'i':'m':'e':':':t, 'D':'i':'s':'t':'a':'n':'c':'e':':':d] =
  (read $ removeSpace t, read $ removeSpace d)
parseRecord _ = (-1, -1)

strategies :: Int -> Int -> (Int, Int) -> Int
strategies t v (rt, rd)
  | t >= rt = 0
  | t' * v > rd = 1 + s'
  | otherwise = s'
  where t' = rt - t
        s' = strategies (t + 1) (v + 1) (rt, rd)

strategiesProd :: [(Int, Int)] -> Int
strategiesProd [] = 1
strategiesProd ((rt, rd):rs) = par s' (pseq ss' (s' * ss'))
  where s'  = strategies 1 1 (rt, rd)
        ss' = strategiesProd rs

main :: IO ()
main = do input <- getLines
          case runParser parseRecords () "" (collapseInput input) of
                Right rs -> do print $ strategiesProd rs
                               print $ strategies 1 1 $ parseRecord input
                Left err -> print err
