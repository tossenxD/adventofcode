module Main where

import Text.ParserCombinators.Parsec
import Common.Parsing
import Data.Char
import GHC.Conc

parseRecords :: Parser [(Int,Int)]
parseRecords = do spaces; symbol "Time:"; ts <- many1 digits
                  symbol "Distance:"; ds <- many1 digits
                  return $ zip ts ds

parseRecord :: [String] -> (Int, Int)
parseRecord ['T':'i':'m':'e':':':t, 'D':'i':'s':'t':'a':'n':'c':'e':':':d] =
  (read $ removeSpace t, read $ removeSpace d)
parseRecord _ = (-1, -1)

strategiesNaive :: Int -> Int -> (Int, Int) -> Int
strategiesNaive t v (rt, rd)
  | t >= rt = 0
  | t' * v > rd = 1 + s'
  | otherwise = s'
  where t' = rt - t
        s' = strategiesNaive (t + 1) (v + 1) (rt, rd)

strategiesFast :: (Int, Int) -> Int
strategiesFast (t, d) = t - l - t + u - 1
  where pivotL = pivotLower (div t 10) t d
        pivotU = pivotUpper (t - (div t 10)) t d
        l      = findLower pivotL t d
        u      = findUpper pivotU t d

pivotLower :: Int -> Int -> Int -> Int
pivotLower v t d
  | v * (t - v) < d = v
  | otherwise = pivotLower (div v 2) t d

findLower :: Int -> Int -> Int -> Int
findLower v t d
  | v * (t - v) > d = v - 1
  | otherwise = findLower (v + 1) t d

pivotUpper :: Int -> Int -> Int -> Int
pivotUpper v t d
  | v * (t - v) < d = v
  | otherwise = pivotUpper (v + (div (t - v) 2)) t d

findUpper :: Int -> Int -> Int -> Int
findUpper v t d
  | v * (t - v) > d = v + 1
  | otherwise = findUpper (v - 1) t d

strategiesProd :: [(Int, Int)] -> Int
strategiesProd [] = 1
strategiesProd ((rt, rd):rs) = par s' (pseq ss' (s' * ss'))
  where s'  = strategiesNaive 1 1 (rt, rd)
        ss' = strategiesProd rs

main :: IO ()
main = do input <- getLines
          case runParser parseRecords () "" (collapseInput input) of
                Right rs -> do print $ strategiesProd rs
                               print $ strategiesFast $ parseRecord input
                Left err -> print err
