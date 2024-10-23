module Main where

import Common.Parsing
import Data.Char
import Data.Map (Map,(!))
import qualified Data.Map as Map

type Network = Map String (String, String)
type Node = (String, (String, String))
data Seq = L Seq | R Seq | E
  deriving (Show, Eq)

parseLines :: [String] -> (Seq, Network)
parseLines ss = (seq, nodes)
  where seq = parseSeq $ head ss
        nodes = foldr (\n -> Map.insert (fst n) (snd n)) Map.empty $
                  map parseNode $ drop 2 ss

parseSeq :: String -> Seq
parseSeq ('L':s) = L $ parseSeq s
parseSeq ('R':s) = R $ parseSeq s
parseSeq _ = E

parseNode :: String -> Node
parseNode s = (name, (left, right))
  where name  = parseNodeName s
        left  = parseNodeName $ drop 7 s
        right = parseNodeName $ drop 12 s

parseNodeName :: String -> String
parseNodeName (a:b:c:_) = a:b:c:[]
parseNodeName _ = error "Not a nodename"

traverseSequence :: String -> (Seq, Seq) -> Network -> Int
traverseSequence name (seq, seq') network
  | name == "ZZZ" = 0
  | seq == E = traverseSequence name (seq', seq') network
  | L seq'' <- seq = 1 + traverseSequence left (seq'', seq') network
  | R seq'' <- seq = 1 + traverseSequence right (seq'', seq') network
  where (left, right) = network ! name

findStarterNodes :: Network -> [String]
findStarterNodes = (filter (("A" ==) . (drop 2))) . Map.keys

traverseSequenceGhost :: [String] -> (Seq, Seq) -> Network -> Int
traverseSequenceGhost names (seq, seq') network
  | endinZs = 0
  | seq == E = traverseSequenceGhost names (seq', seq') network
  | L seq'' <- seq = 1 + traverseSequenceGhost (mov (fst)) (seq'', seq') network
  | R seq'' <- seq = 1 + traverseSequenceGhost (mov (snd)) (seq'', seq') network
  where mov f = map (\n -> f (network ! n)) names
        endinZs = foldr (\n acc -> acc && drop 2 n == "Z") True names

main :: IO ()
main = do input <- getLinesStop
          print $ (\(s,n) -> traverseSequence "AAA" (s,s) n) $ parseLines input
          print $ (\(s,n) -> traverseSequenceGhost (findStarterNodes n) (s,s) n) $
            parseLines input
