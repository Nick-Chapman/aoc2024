module Day10 (main) where

import Data.Map (Map)
import Misc (check,nub)
import Par4 (parse,Par,separated,nl,many,digit)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day10.sample"
  inp <- parse gram <$> readFile "input/day10.input"
  print ("day10, part1 (sample)", check 36 $ part1 sam)
  print ("day10, part1", check 786 $ part1 inp)
  print ("day10, part1 (sample)", check 81 $ part2 sam)
  print ("day10, part1", check 1722 $ part2 inp)

type Top = Map Pos Int
type Pos = (Int,Int)

gram :: Par Top
gram = makeTop <$> separated nl (many digit)

makeTop :: [[Int]] -> Top
makeTop dss = Map.fromList [ ((x,y),d) | (y,ds) <- zip [0..] dss, (x,d) <- zip [0..] ds ]

part1 :: Top -> Int
part1 top = sum [ length (nub (hike top 0 h)) | h <- trailheads top ]

part2 :: Top -> Int
part2 top = sum [ length (hike top 0 h) | h <- trailheads top ]

trailheads :: Top -> [Pos]
trailheads top = [ p | (p,0) <- Map.toList top ]

hike :: Top -> Int -> Pos -> [Pos]
hike top n p = do
  if n == 9 then [p] else do
    [ d
      | q <- adj p, Map.lookup q top == Just (n+1)
      , d <- hike top (n+1) q
      ]

adj :: Pos -> [Pos]
adj (x,y) = [ (x,y-1), (x,y+1), (x-1,y), (x+1,y) ]
