module Day8 (main) where

import Misc (check,nub,collate)
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
  sam <- parse <$> readFile "input/day8.sample"
  inp <- parse <$> readFile "input/day8.input"
  print ("day8, part1 (sample)", check 14 $ part1 sam)
  print ("day8, part1", check 252 $ part1 inp)
  print ("day8, part2 (sample)", check 34 $ part2 sam)
  print ("day8, part2", check 839 $ part2 inp)

type Freq = Char
type Pos = (Int,Int)
data Input = Input { max :: Pos, m :: Map Pos Freq } deriving Show

parse :: String -> Input
parse s = do
  let max = (length (head (lines s)), length (lines s))
  let m = Map.fromList
        [ ((x,y),c) | (y,line) <- zip [0..] (lines s) , (x,c) <- zip [0..] line , c /= '.' ]
  Input { max, m }

part1 :: Input -> Int
part1 Input{max=(maxX,maxY),m} = do
  let onGrid (x,y) = x>=0 && y>=0 && x<maxX && y<maxY
  (length . nub)
    [ a
    | (_,ps) <- Map.toList (byFreq m)
    , pp <- pairs ps
    , a <- antiNodes pp
    , onGrid a
    ]

part2 :: Input -> Int
part2 Input{max=(maxX,maxY),m} = do
  let onGrid (x,y) = x>=0 && y>=0 && x<maxX && y<maxY
  (length . nub)
    [ a
    | (_,ps) <- Map.toList (byFreq m)
    , pp <- pairs ps
    , a <- antiNodes2 onGrid pp
    ]

byFreq :: Map Pos Freq -> Map Freq [Pos]
byFreq m = Map.fromList (collate [ (f,p) | (p,f) <- Map.toList m ])

pairs :: [a] -> [(a,a)]
pairs xs = [ (x,y) | x:ys <- tails xs, y <- ys ]

tails :: [a] -> [[a]]
tails = \case [] -> [[]]; xs@(_:xs') -> xs : tails xs'

antiNodes :: (Pos,Pos) -> [Pos]
antiNodes (p1,p2) = [ p2 `add` (p2 `minus` p1), p1 `add` (p1 `minus` p2) ]

minus :: Pos -> Pos -> Pos
minus (x1,y1) (x2,y2) = (x1-x2,y1-y2)

add :: Pos -> Pos -> Pos
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

antiNodes2 ::  (Pos -> Bool) -> (Pos,Pos) ->[Pos]
antiNodes2 onGrid (p1,p2) =
  loop p2 (p2 `minus` p1) ++ loop p1 (p1 `minus` p2)
  where
    loop :: Pos -> Pos -> [Pos]
    loop p step = if onGrid p then p : loop (p `add` step) step else []
