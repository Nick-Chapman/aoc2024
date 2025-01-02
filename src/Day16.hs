module Day16 (main) where

import Data.Map (Map)
import Data.Set (Set)
import Misc (check,the,nub)
import Par4 (parse,Par,separated,nl,some,alts,lit)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.sample"
  sam2 <- parse gram <$> readFile "input/day16.sample2"
  inp <- parse gram <$> readFile "input/day16.input"
  print ("day16, part1 (sample)", check 7036 $ part1 sam)
  print ("day16, part1 (sample2)", check 11048 $ part1 sam2)
  print ("day16, part1", check 130536 $ part1 inp)
  print ("day16, part2 (sample)", check 45 $ part2 sam)
  print ("day16, part2 (sample2)", check 64 $ part2 sam2)
  print ("day16, part2", check 1024 $ part2 inp)

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

type Node = (Pos,Dir)
data Dir = N | E | S | W deriving (Eq,Ord,Show)
type Cost = Int

part1 :: Input -> Int
part1 inp = do
  let start = the [ p | (p,Start) <- Map.toList inp ]
  let end = the [ p | (p,End) <- Map.toList inp ]
  let isFinished (p,_) = (p==end)
  let (cost,_) = search (start,E) isFinished (step inp)
  cost

part2 :: Input -> Int
part2 inp = do
  let start = the [ p | (p,Start) <- Map.toList inp ]
  let end = the [ p | (p,End) <- Map.toList inp ]
  let isFinished (p,_) = (p==end)
  let (_,coverage) = search (start,E) isFinished (step inp)
  length $ nub [ pos | (pos,_) <- Set.toList coverage ]

step :: Input -> Node -> [(Node,Cost)]
step i (p,d) =
  (let p1 = forward p d in if isWall p1 then [] else [((p1,d),1)]) ++
  (let d1 = anti d in let p1 = forward p d1 in if isWall p1 then [] else [((p1,d1),1001)]) ++
  (let d1 = clock d in let p1 = forward p d1 in if isWall p1 then [] else [((p1,d1),1001)])
  where isWall p = Map.lookup p i == Just Wall

clock :: Dir -> Dir
clock = \case N -> E; E -> S; S -> W; W -> N

anti :: Dir -> Dir
anti = \case N -> W; E -> N; S -> E; W -> S

forward :: Pos -> Dir -> Pos
forward (x,y) = \case
  N -> (x,y-1)
  E -> (x+1,y)
  S -> (x,y+1)
  W -> (x-1,y)

type Frontier = [(Cost,[(Node,Coverage)])] -- ordered by cost
type Coverage = Set Node

search :: Node -> (Node -> Bool) -> (Node -> [(Node,Cost)]) -> (Cost, Coverage)
search node0 isFinished step = loop 0 Set.empty start
  where
    start :: Frontier
    start = [(0,[(node0,Set.empty)])]

    loop :: Int -> Set Node -> Frontier -> (Cost,Coverage)
    loop i acc = \case
      [] -> error "search"
      (cost,ncs):frontier -> do
        case [ p | p@(node,_) <- ncs , isFinished node ] of
          ps@(_:_) -> (cost, Set.unions [ Set.insert node coverage | (node,coverage) <- ps ])
          [] -> do
            let
              trips :: [(Cost,(Node,Coverage))]
              trips =
                [ (cost+cost1, (node', Set.insert node coverage))
                | (node,coverage) <- ncs
                , (node',cost1) <- step node
                , node' `Set.notMember` acc
                ]
            let acc' = acc `Set.union` Set.fromList (map fst ncs)
            let frontier' = foldl insertFrontier frontier trips
            loop (i+1) acc' frontier'

insertFrontier :: Frontier -> (Cost,(Node,Coverage)) -> Frontier
insertFrontier xs (trip@(cost,nc)) = case xs of
  [] -> [(cost,[nc])]
  xs@(x@(cost1,ncs):xs') ->
    if cost < cost1 then ((cost,[nc]):xs) else
      if cost == cost1 then (cost,nc:ncs) : xs' else
        x : insertFrontier xs' trip
