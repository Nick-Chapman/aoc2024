module Day10 (main) where

import Data.Map (Map)
import Data.Set (Set)
import Misc (check)
import Par4 (parse,Par,separated,nl,many,digit)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day10.sample"
  inp <- parse gram <$> readFile "input/day10.input"
  print ("day10, part1 (sample)", check 36 $ part1 sam)
  print ("day10, part1", check 786 $ part1 inp)

type Top = Map Pos Int
type Pos = (Int,Int)

gram :: Par Top
gram = makeTop <$> separated nl (many digit)

makeTop :: [[Int]] -> Top
makeTop dss = do
  Map.fromList [ ((x,y),d) | (y,ds) <- zip [0..] dss, (x,d) <- zip [0..] ds ]

part1 :: Top -> Int
part1 top =
  sum [ Set.size dest | h <- trailheads top , let dest = hike top 0 (Set.singleton h) ]

trailheads :: Top -> [Pos]
trailheads top = [ p | (p,0) <- Map.toList top ]

hike :: Top -> Int -> Set Pos -> Set Pos
hike top n ps = do
  if n == 9 then ps else do
    hike top (n+1) $
      Set.fromList [ q | p <- Set.toList ps, q <- adj p, Map.lookup q top == Just (n+1) ]

adj :: Pos -> [Pos]
adj (x,y) = [ (x,y-1), (x,y+1), (x-1,y), (x+1,y) ]
