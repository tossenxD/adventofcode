module Main where

import Data.Char
import Common.Parsing

data Schematic = Symbol Char Int | Number Int (Int,Int) | Part Int (Int,Int)
  deriving (Read, Show, Eq)

getSymbolPos :: [Schematic] -> [Int]
getSymbolPos [] = []
getSymbolPos (Symbol s p : schs) = p : (getSymbolPos schs)
getSymbolPos (sch:schs) = getSymbolPos schs

getStarPos :: [Schematic] -> [Int]
getStarPos [] = []
getStarPos (Symbol '*' p : schs) = p : (getSymbolPos schs)
getStarPos (sch:schs) = getSymbolPos schs

findDigit :: String -> (Int, Int)
findDigit [] = (0,0)
findDigit (s:ss)
  | isDigit s = (\(n,d) -> (n + ((10^d) * (digitToInt s)),d+1)) $ findDigit ss
  | otherwise = (0,0)

parseSchematic :: Int -> String -> [Schematic]
parseSchematic _ [] = []
parseSchematic p ('.':ss) = parseSchematic (p+1) ss
parseSchematic p (s:ss)
  | isDigit s = (\(n,i) -> (Number n (p,p+i-1)) : (parseSchematic (p+i) (drop (i-1) ss))) $ findDigit (s:ss)
  | otherwise = (Symbol s p) : (parseSchematic (p+1) ss)

padSchematic :: [[Schematic]] -> [[Schematic]]
padSchematic schematics = []:schematics++[[]]

findParts :: [[Schematic]] -> [[Schematic]]
findParts [] = []
findParts [sch] = [sch]
findParts [sch0,sch1] = [sch0,sch1]
findParts (sch0:sch1:sch2:schs) =
  let ps = getSymbolPos sch1
      sch0' = findParts' sch0 ps
      sch1' = findParts' sch1 ps
      sch2' = findParts' sch2 ps
  in sch0' : (findParts (sch1':sch2':schs))

findParts' :: [Schematic] -> [Int] -> [Schematic]
findParts' schs [] = schs
findParts' schs (p:ps) = findParts' (findParts'' schs p) ps

findParts'' :: [Schematic] -> Int -> [Schematic]
findParts'' [] _ = []
findParts'' (Number n (i0,i1) : schs) p
  | i0 <= p+1 && p-1 <= i1 = (Part n (i0,i1)) : (findParts'' schs p)
  | otherwise = (Number n (i0,i1)) : (findParts'' schs p)
findParts'' (sch : schs) p =  sch : (findParts'' schs p)

partSum :: [Schematic] -> Int
partSum [] = 0
partSum (Part n _: schs) = n + (partSum schs)
partSum (sch:schs) = partSum schs

gearRatio :: [[Schematic]] -> [Int]
gearRatio [] = []
gearRatio [sch] = []
gearRatio [sch0,sch1] = []
gearRatio (sch0:sch1:sch2:schs) =
  (filter ((/=) (-1)) $ map (gearRatio' sch0 sch1 sch2) $ getStarPos sch1) ++ (gearRatio (sch1:sch2:schs))

gearRatio' :: [Schematic] -> [Schematic] -> [Schematic] -> Int -> Int
gearRatio' sch0 sch1 sch2 p = if length prts /= 2 then -1 else foldr (*) 1 prts
  where prts = (gearRatio'' sch0 p)++(gearRatio'' sch1 p)++(gearRatio'' sch2 p)

gearRatio'' :: [Schematic] -> Int -> [Int]
gearRatio'' [] p = []
gearRatio'' (Part n (i0,i1):schs) p
  | i0 <= p+1 && p-1 <= i1 = n:(gearRatio'' schs p)
  | otherwise = gearRatio'' schs p
gearRatio'' (_:schs) p =  gearRatio'' schs p

main = do input <- getLines
          print $ foldr (+) 0 $ map partSum $ findParts $ padSchematic $ map (parseSchematic 0) input
          print $ foldr (+) 0 $ gearRatio $ findParts $ padSchematic $ map (parseSchematic 0) input
