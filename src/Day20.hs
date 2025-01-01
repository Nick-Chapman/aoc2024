module Day20 (main) where

import Misc (check,the)
import Par4 (parse,Par,separated,nl,lit,some,alts)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day20.sample"
  inp <- parse gram <$> readFile "input/day20.input"
  print ("day20, part1 (sample)", check 44 $ part1 0 sam)
  print ("day20, part1", check 1415 $ part1 100 inp)

type Input = Map Pos Tile
type Pos = (Int,Int)
data Tile = Start | End | Wall | Path deriving (Eq,Show)

gram :: Par Input
gram = makeInput <$> maze
  where
    maze = separated nl (some tile)
    tile = alts
      [ do lit '#'; pure Wall
      , do lit '.'; pure Path
      , do lit 'S'; pure Start
      , do lit 'E'; pure End ]

makeInput :: [[Tile]] -> Input
makeInput tss = Map.fromList [ ((x,y),t) | (y,ts) <- zip [0..] tss, (x,t) <- zip [0..] ts ]

part1 :: Int -> Input -> Int
part1 min inp = do
  let r :: Map Pos Int = Map.fromList (zip (findBest inp) [0..])
  length
    [ score
    | (p,i) <- Map.toList r
    , p1 <- adj p
    , p2 <- adj p1
    , score <- case Map.lookup p2 r of Nothing -> []; Just j -> if j > i+3 then [j-i-2] else []
    , score >= min
    ]

findBest :: Input -> [Pos]
findBest inp = do
  let
    look :: Pos -> Tile
    look p = maybe undefined id $ Map.lookup p inp
  let
    start :: Pos
    start = the [ p | (p,Start) <- Map.toList inp ]
  let
    step :: (Pos -> Bool) -> Pos -> Pos
    step isLast p = the [ q | q <- adj p , case look q of Path -> not (isLast q); End -> True; _ -> False ]
  let
    collect :: (Pos -> Bool) -> Pos -> [Pos]
    collect isLast pos = do
      case look pos of
        End -> [pos]
        Wall -> undefined
        _ -> pos : collect (==pos) (step isLast pos)
  collect (const False) start

adj :: Pos -> [Pos]
adj (x,y) = [ (x,y-1), (x,y+1), (x-1,y), (x+1,y) ]
