module Main where

import Data.Char
import Parsing

isSpelledDigit :: String -> Int
isSpelledDigit ('z':'e':'r':'o'    :_) = 0
isSpelledDigit ('o':'n':'e'        :_) = 1
isSpelledDigit ('t':'w':'o'        :_) = 2
isSpelledDigit ('t':'h':'r':'e':'e':_) = 3
isSpelledDigit ('f':'o':'u':'r'    :_) = 4
isSpelledDigit ('f':'i':'v':'e'    :_) = 5
isSpelledDigit ('s':'i':'x'        :_) = 6
isSpelledDigit ('s':'e':'v':'e':'n':_) = 7
isSpelledDigit ('e':'i':'g':'h':'t':_) = 8
isSpelledDigit ('n':'i':'n':'e'    :_) = 9
isSpelledDigit _                       = -1

calibrate :: String -> Int
calibrate [] = -1
calibrate (s:ss)
  | isDigit s = (+) (s' * 10)  $ if ss' == -1 then s'  else ss'
  | sp' /= -1 = (+) (sp' * 10) $ if ss' == -1 then sp' else ss'
  | otherwise = calibrate ss
    where ss' = calibrate' ss
          s'  = digitToInt s
          sp' = isSpelledDigit (s:ss)

calibrate' :: String -> Int
calibrate' [] = -1
calibrate' (s:ss)
  | isDigit s = if ss' == -1 then digitToInt s else ss'
  | sp' /= -1 = if ss' == -1 then sp' else ss'
  | otherwise = ss'
    where ss' = calibrate' ss
          sp' = isSpelledDigit (s:ss)

main = do input <- getLines
          print $ foldr (+) 0 $ map calibrate input
