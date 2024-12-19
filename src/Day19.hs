module Day19 (main) where

import Misc (check)
import Par4 (parse,Par,key,word,separated,nl)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day19.sample"
  inp <- parse gram <$> readFile "input/day19.input"
  print ("day19, part1 (sample)", check 6 $ part1 sam)
  print ("day19, part1", check 324 $ part1 inp)
  print ("day19, part2 (sample)", check 16 $ part2 sam)
  print ("day19, part2", check 575227823167869 $ part2 inp)

data Input = Input { towels :: [String], designs :: [String] } deriving Show

gram :: Par Input
gram = do
  towels <- separated (key ", ") word; nl; nl
  designs <- separated nl word
  pure Input {towels,designs}

part1 :: Input -> Int
part1 Input{towels,designs} =
  sum [ min (countWays towels d) 1 | d <- designs ]

part2 :: Input -> Int
part2 Input{towels,designs} =
  sum [ countWays towels d | d <- designs ]

countWays :: [String] -> String -> Int
countWays ps design = consume design
  where
    consume' s = maybe undefined id $ Map.lookup s m -- memoized
      where m = Map.fromList [ (k,consume k) | k <- tails design ]
    consume :: String -> Int
    consume "" = 1
    consume x = sum [ consume' y | p <- ps, y <- dropPrefix p x ]

dropPrefix :: String -> String -> [String]
dropPrefix = \case
  [] -> \s -> [s]
  x:xs -> \case
    [] -> []
    y:ys -> if x == y then dropPrefix xs ys else []

tails :: [a] -> [[a]]
tails = \case [] -> [[]]; xs@(_:xs') -> xs : tails xs'
