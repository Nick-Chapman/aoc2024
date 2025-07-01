module Day6 (main) where

import Data.Set (Set,member,notMember)
import Misc (check,the,nub,head)
import Prelude hiding (head)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse <$> readFile "input/day6.sample"
  inp <- parse <$> readFile "input/day6.input"
  print ("day6, part1 (sample)", check 41 $ part1 sam)
  print ("day6, part1", check 5318 $ part1 inp)

  part2_sam <- part2 sam
  print ("day6, part2 (sample)", check 6 $ part2_sam)

  part2_inp <- part2 inp -- (approx 6 mins)
  print ("day6, part2", check 1831 $ part2_inp)

type Pos = (Int,Int)
data Dir = N | E | S | W deriving (Eq,Ord,Show)
type PD = (Pos, Dir)
type Person = PD
type Blocks = Set Pos
type Size = Pos
type Input = (Size, Blocks, Person)

frontOfMe :: Pos -> Dir -> Pos
frontOfMe (x,y) = \case
  N -> (x,y-1)
  E -> (x+1,y)
  S -> (x,y+1)
  W -> (x-1,y)

turn90 :: Dir -> Dir
turn90 = \case N -> E; E -> S; S -> W; W -> N

parse :: String -> Input
parse s = do
  let size = (length (head (lines s)), length (lines s))
  let trips = [ (x,y,c) | (y,line) <- zip [0..] (lines s) , (x,c) <- zip [0..] line ]
  let blocks = Set.fromList [ (x,y) | (x,y,'#') <- trips ]
  let person = the [ ((x,y),N) | (x,y,'^') <- trips ]
  (size,blocks,person)

part1 :: Input -> Int
part1 i = Set.size $ Set.fromList [ p | (p,_) <- path i ]

part2 :: Input -> IO Int
part2 i = do
  let (_,_,(personPos,_)) = i
  let xs = nub [ p | (p,_) <- path i, p /= personPos ]
  --print (length xs)
  check 0 0 xs
  where
    check :: Int -> Int -> [Pos] -> IO Int
    check n y = \case
      [] -> pure y
      extra:xs -> do
        --print (n,y)
        if makesLoopy (addObstacle i extra)
        then check (n+1) (y+1) xs
        else check (n+1) y xs

makesLoopy :: Input -> Bool
makesLoopy i = isLoopy Set.empty (path i)
  where
    isLoopy :: Set PD -> [PD] -> Bool
    isLoopy visited = \case
      [] -> False
      pd:path -> do
        if pd `member` visited then True else
          isLoopy (Set.insert pd visited) path

addObstacle :: Input -> Pos -> Input
addObstacle (max,blocks,person) x = (max, Set.insert x blocks, person)

path :: Input -> [PD]
path ((maxX,maxY),blocks,person) = loop person
  where
    offGrid :: Pos -> Bool
    offGrid (x,y) = x < 0 || y < 0 || x >= maxX || y >= maxY

    loop :: Person -> [PD]
    loop pd@(pos,dir) =
      if offGrid pos then [] else do
        let pos1 = frontOfMe pos dir
        let pd' = if pos1 `notMember` blocks then (pos1,dir) else (pos,turn90 dir)
        pd : loop pd'
