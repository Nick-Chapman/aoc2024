module Day21 (main) where

import Misc (check)

main :: IO ()
main = do
  sam <- lines <$> readFile "input/day21.sample"
  inp <- lines <$> readFile "input/day21.input"
  print ("day21, part1 (sample)", check 126384 $ part1 sam)
  print ("day21, part1", check 171596 $ part1 inp)

data K2 = U | A | L | D | R

part1 :: [String] -> Int
part1 = sum . map complexity

complexity :: String -> Int
complexity s = do
  let ns = [ length xs | xs <- typings s ]
  let i = read @Int (init s)
  let j = minimum ns
  (i*j)

typings :: String -> [[K2]]
typings s =
  [ zs
  | xs <- type1 'A' s
  , ys <- type2 A xs
  , zs <- type2 A ys
  ]

type1 :: Char -> String -> [[K2]]
type1 c = \case
  [] -> [[]]
  d:ds -> [ xs ++ [A] ++ ys | xs <- moveK1 c d , ys <- type1 d ds]

type2 :: K2 -> [K2] -> [[K2]]
type2 c = \case
  [] -> [[]]
  d:ds -> [ xs ++ [A] ++ ys | xs <- moveK2 c d , ys <- type2 d ds]

moveK1 :: Char -> Char -> [[K2]]
moveK1 c d = do
  let (cx,cy) = posK1 c
  let (dx,dy) = posK1 d
  let x = dx - cx
  let y = dy - cy
  let vh = vert y ++ hori x
  let hv = hori x ++ vert y
  if (cx==0 && dy==3) then [hv] else
    if (cy==3 && dx==0) then [vh] else
      [hv,vh]

moveK2 :: K2 -> K2 -> [[K2]]
moveK2 c d = do
  let (cx,cy) = posK2 c
  let (dx,dy) = posK2 d
  let x = dx - cx
  let y = dy - cy
  let vh = vert y ++ hori x
  let hv = hori x ++ vert y
  if (cx==0 && dy==0) then [hv] else
    if (cy==0 && dx==0) then [vh] else
      [hv
      -- ,vh
      ]

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
