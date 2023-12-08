module Main where

import Text.ParserCombinators.Parsec
import Parsing
import Data.Char
import Control.Parallel

type Description = (Int, Int, Int)
type Map = (Int -> Int)
type Almanac = ([Int], Map, Map, Map, Map, Map, Map, Map)

initMap :: Map
initMap = (\n -> n)

extendMap :: Description -> Map -> Map
extendMap (dst, src, r) m =
  (\n -> if src <= n && n <= src + r - 1 then n - (src - dst) else m n)

parseAlmanac :: Parser Almanac
parseAlmanac = do spaces; symbol "seeds:"; seeds <- many1 digits
                  symbol "seed-to-soil map:"; m1 <- many1 parseMapping
                  symbol "soil-to-fertilizer map:"; m2 <- many1 parseMapping
                  symbol "fertilizer-to-water map:"; m3 <- many1 parseMapping
                  symbol "water-to-light map:"; m4 <- many1 parseMapping
                  symbol "light-to-temperature map:"; m5 <- many1 parseMapping
                  symbol "temperature-to-humidity map:"; m6 <- many1 parseMapping
                  symbol "humidity-to-location map:"; m7 <- many1 parseMapping
                  return (seeds, toMap m1, toMap m2, toMap m3, toMap m4, toMap m5, toMap m6, toMap m7)
  where toMap = foldr (extendMap) initMap

parseMapping :: Parser Description
parseMapping = do dst <- digits; src <- digits; r <- digits; return (dst, src, r)

runAlmanac :: Almanac -> [Int]
runAlmanac ([], _, _, _, _, _, _, _) = []
runAlmanac (s:ss, m1, m2, m3, m4, m5, m6, m7) = par s' (pseq alm' (s':alm'))
  where s' = m7 $ m6 $ m5 $ m4 $ m3 $ m2 $ m1 s
        alm' = runAlmanac (ss, m1, m2, m3, m4, m5, m6, m7)

runAlmanacRange :: Almanac -> Int
runAlmanacRange (ss, m1, m2, m3, m4, m5, m6, m7) = applyExpansion (m7 . m6 . m5 . m4 . m3 . m2 . m1) ss

applyExpansion :: (Int -> Int) -> [Int] -> Int
applyExpansion _ [] = -1
applyExpansion f [s] = f s
applyExpansion f (s1:s2:ss)
  | l2 == -1 = l1
  | otherwise = min l1 l2
  where l1 = minimum $ map f [s1..(s1 + s2 - 1)]
        l2 = applyExpansion f ss

main :: IO ()
main = do input <- getLinesStop
          case runParser parseAlmanac () "" (collapseInput input) of
                Right alm -> do print $ minimum $ runAlmanac alm
                                print $ runAlmanacRange alm
                Left err -> print err
