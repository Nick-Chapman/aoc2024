module Day18 (main) where

import Misc (check)
import Par4 (parse,Par,int,separated,nl,lit)
import Data.Set (Set,union,notMember)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day18.sample"
  inp <- parse gram <$> readFile "input/day18.input"
  print ("day18, part1 (sample)", check 22 $ part1 12 (6,6) sam)
  print ("day18, part1", check 270 $ part1 1024 (70,70) inp)
  print ("day18, part2 (sample)", check (6,1) $ part2 (6,6) sam)
  print ("day18, part2", check (51,40) $ part2 (70,70) inp)

type Input = [Pos]
type Pos = (Int,Int)

gram :: Par Input
gram = separated nl pos
  where pos = do x <- int; lit ','; y <- int; pure (x,y)

part1 :: Int -> Pos -> Input -> Int
part1 n goal xs =
  case search n goal xs of
    Just n -> n
    Nothing -> undefined

part2 :: Pos -> Input -> Pos
part2 goal xs = do
  head [ xs!!n | n <- reverse [0..length xs], Just{} <- [ search n goal xs ] ]

search :: Int -> Pos -> Input -> Maybe Int
search n goal xs = do
  let (gX,gY) = goal
  let init = (0,0)
  let corrupted = Set.fromList (take n xs)
  let inBounds (x,y) = x>=0 && y>=0 && x<=gX && y <=gY
  let
    step :: Pos -> [Pos]
    step p = [ q | q <- adj p, inBounds q, q `Set.notMember` corrupted ]
  shortest init step goal

adj :: Pos -> [Pos]
adj (x,y) = [ (x,y-1), (x,y+1), (x-1,y), (x+1,y) ]

shortest :: Pos -> (Pos -> [Pos]) -> Pos -> Maybe Int
shortest init step goal = loop 0 (Set.singleton init) Set.empty
  where
    loop :: Int -> Set Pos -> Set Pos -> Maybe Int
    loop i frontier acc  = do
     if Set.null frontier then Nothing else
      if goal `Set.member` frontier then Just i else do
        let
          nextFrontier =
            Set.fromList
            [ q
            | p <- Set.toList frontier
            , q <- step p
            , q `notMember` acc
            ]
        let nextAcc = acc `union` frontier
        loop (i+1) nextFrontier nextAcc
