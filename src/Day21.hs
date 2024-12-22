module Day21 (main) where

import Misc (check)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- lines <$> readFile "input/day21.sample"
  inp <- lines <$> readFile "input/day21.input"
  print ("day21, part1 (sample)", check 126384 $ part1 sam)
  print ("day21, part1", check 171596 $ part1 inp)
  print ("day21, part2", check 209268004868246 $ part2 inp)
    where
      part1 = go 2
      part2 = go 25

data K2 = U | A | L | D | R deriving (Eq,Ord,Show)

go :: Int -> [String] -> Int
go k = sum . map (complexity k)

complexity :: Int -> String -> Int
complexity k s = do
  let i = read @Int (init s)
  let n = shortestTypings k s
  (i*n)

shortestTypings :: Int -> String -> Int
shortestTypings kMax s = sum [ shortestMove1 kMax (c,d) | (c,d) <- zip ('A':s) s ]
  where
    shortestMove1 :: Int -> (Char,Char) -> Int
    shortestMove1 k (c,d) = minimum [ lengthArrowTypings k xs | xs <- moveK1 c d ]

    lengthArrowTypings :: Int -> [K2] -> Int
    lengthArrowTypings k xs = case k of
      0 -> length xs
      _ -> sum [ shortestMove2' (k-1) (c,d) | (c,d) <- zip (A:xs) xs ]

    shortestMove2' :: Int -> (K2,K2) -> Int
    shortestMove2' k (c,d) = maybe err id $ Map.lookup (k,c,d) m
      where err = error (show ("look",k,c,d))

    m :: Map (Int,K2,K2) Int
    m = Map.fromList
      [ ((k,c,d), shortestMove2 k (c,d))
      | k <- [0..kMax-1]
      , c <- [U,A,L,D,R]
      , d <- [U,A,L,D,R]
      ]

    shortestMove2 :: Int -> (K2,K2) -> Int
    shortestMove2 k (c,d) = minimum [ lengthArrowTypings k xs | xs <- moveK2 c d ]

moveK1 :: Char -> Char -> [[K2]]
moveK1 c d = do
  let (cx,cy) = posK1 c
  let (dx,dy) = posK1 d
  let x = dx - cx
  let y = dy - cy
  let vh = vert y ++ hori x ++ [A]
  let hv = hori x ++ vert y ++ [A]
  if (cx==0 && dy==3) then [hv] else
    if (cy==3 && dx==0) then [vh] else [hv,vh]

moveK2 :: K2 -> K2 -> [[K2]]
moveK2 c d = do
  let (cx,cy) = posK2 c
  let (dx,dy) = posK2 d
  let x = dx - cx
  let y = dy - cy
  let vh = vert y ++ hori x ++ [A]
  let hv = hori x ++ vert y ++ [A]
  if (cx==0 && dy==0) then [hv] else
    if (cy==0 && dx==0) then [vh] else [hv,vh]

hori :: Int -> [K2]
hori n = if n == 0 then [] else if n < 0 then replicate (abs n) L else replicate n R

vert :: Int -> [K2]
vert n = if n == 0 then [] else if n < 0 then replicate (abs n) U else replicate n D

type Pos = (Int,Int)

posK1 :: Char -> Pos
posK1 = \case
  '7' -> (0,0)
  '8' -> (1,0)
  '9' -> (2,0)
  '4' -> (0,1)
  '5' -> (1,1)
  '6' -> (2,1)
  '1' -> (0,2)
  '2' -> (1,2)
  '3' -> (2,2)
  '0' -> (1,3)
  'A' -> (2,3)
  _ -> undefined

posK2 :: K2 -> Pos
posK2 = \case
  U -> (1,0)
  A -> (2,0)
  L -> (0,1)
  D -> (1,1)
  R -> (2,1)
