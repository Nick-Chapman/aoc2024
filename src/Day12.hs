module Day12 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,many,dot)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set,empty,(\\),union,singleton,notMember)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day12.sample"
  inp <- parse gram <$> readFile "input/day12.input"
  print ("day12, part1 (sample)", check 1930 $ part1 sam)
  print ("day12, part1", check 1449902 $ part1 inp)
  print ("day12, part2 (sample)", check 1206 $ part2 sam)
  print ("day12, part2", check 908042 $ part2 inp)
  where
    part1 = go Part1
    part2 = go Part2

data Part = Part1 | Part2

type PlantType = Char
type Pos = (Int,Int)
type Grid = Map Pos PlantType

gram :: Par Grid
gram = makeGrid <$> separated nl (many dot)

makeGrid :: [[PlantType]] -> Grid
makeGrid pss = Map.fromList [ ((x,y),p) | (y,ps) <- zip [0..] pss, (x,p) <- zip [0..] ps ]

go :: Part -> Grid -> Int
go part g = loop 0 0 empty
  where
    countBorders =
      case part of
        Part1 -> countBorders1 g
        Part2 -> countBorders2 g

    loop :: Int -> Int -> Set Pos -> Int
    loop acc i covered = do
      let uncovered = all \\ covered
      case Set.lookupMin uncovered of
        Nothing -> acc
        Just pos -> do
          let thisPlant = maybe undefined id $ Map.lookup pos g
          let bed = discoverPlantBedContaining thisPlant g pos
          let area = Set.size bed
          let perim = sum [ countBorders thisPlant p | p <- Set.toList bed ]
          let acc' = acc + area*perim
          loop acc' (i+1) (covered `union` bed)

    all :: Set Pos
    all = (Set.fromList . Map.keys) g

discoverPlantBedContaining :: PlantType -> Grid -> Pos -> Set Pos
discoverPlantBedContaining thisPlant g start = discover 0 (singleton start) empty
  where
    discover :: Int -> Set Pos -> Set Pos -> Set Pos
    discover i frontier acc  = do
      case Set.null frontier of
        True -> acc
        False -> do
          let nextFrontier = Set.fromList
                [ q
                | p <- Set.toList frontier
                , q <- adj p
                , q `notMember` acc
                , Just ty <- [Map.lookup q g]
                , ty == thisPlant
                ]
          let nextAcc = acc `union` frontier
          discover (i+1) nextFrontier nextAcc

countBorders1 :: Grid -> PlantType -> Pos -> Int
countBorders1 g ty p =
  length [ ()
         | q <- adj p
         , case Map.lookup q g of Just ty2 -> ty /= ty2; Nothing -> True
         ]

adj :: Pos -> [Pos]
adj (x,y) = [ (x,y-1), (x,y+1), (x-1,y), (x+1,y) ]

countBorders2 :: Grid -> PlantType -> Pos -> Int
countBorders2 g ty p =
  length [ ()
         | dir <- [N,E,S,W]
         , let q = step p dir
         , let bq = case Map.lookup q g of Just ty2 -> ty /= ty2; Nothing -> True
         , let r = step p (turn dir)
         , let br = case Map.lookup r g of Just ty2 -> ty /= ty2; Nothing -> True
         , let s = step r dir
         , let bs = case Map.lookup s g of Just ty2 -> ty /= ty2; Nothing -> True
         , bq && not( not br && bs)
         ]

data Dir = N | E | S | W

turn :: Dir -> Dir
turn = \case
  N -> W
  E -> N
  S -> E
  W -> S

step :: Pos -> Dir -> Pos
step (x,y) = \case
  N -> (x,y-1)
  E -> (x+1,y)
  S -> (x,y+1)
  W -> (x-1,y)
