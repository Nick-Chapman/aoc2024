module Day21 (main) where

import Misc (check)

main :: IO ()
main = do
  let _ = play

  play "029A"
  play "980A"
  play "179A"
  play "456A"
  play "379A"

  sam <- lines <$> readFile "input/day21.sample"
  part1_sam <- pure $ part1 sam
  print ("day21, part1 (sample)", check 126384 $ part1_sam)
  --inp <- lines <$> readFile "input/day21.input"
  --print ("day21, part1", check 0 $ part1 inp)

  pure ()

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

data K2 = U | A | L | D | R

instance Show K2 where show = \case U -> "^"; D -> "v"; L -> "<"; R -> ">"; A -> "A"

seeK2s :: [K2] -> String
seeK2s = concat . map show

hori :: Int -> [K2]
hori n = if n == 0 then [] else if n < 0 then replicate (abs n) L else replicate n R

vert :: Int -> [K2]
vert n = if n == 0 then [] else if n < 0 then replicate (abs n) U else replicate n D

posK2 :: K2 -> Pos
posK2 = \case
  U -> (1,0)
  A -> (2,0)
  L -> (0,1)
  D -> (1,1)
  R -> (2,1)


moveK1 :: Char -> Char -> [[K2]]
moveK1 c d = do
  let (cx,cy) = posK1 c
  let (dx,dy) = posK1 d
  let x = dx - cx
  let y = dy - cy
  if x < 0 then [vert y ++ hori x] else
    if y > 0 then [hori x ++ vert y] else
      [ hori x ++ vert y
      , vert y ++ hori x
      ]


moveK2 :: K2 -> K2 -> [[K2]]
moveK2 c d = do
  let (cx,cy) = posK2 c
  let (dx,dy) = posK2 d
  let x = dx - cx
  let y = dy - cy
  if x < 0 then [vert y ++ hori x] else
    if y < 0 then [hori x ++ vert y] else
      [ hori x ++ vert y
      , vert y ++ hori x
      ]
  --[if x < 0 then (vert y ++ hori x) else (hori x ++ vert y)]
  --if y < 0 then (hori x ++ vert y) else (vert y ++ hori x)


type1 :: Char -> String -> [[K2]]
type1 c = \case
  [] -> [[]]
  d:ds ->
    [ xs ++ [A] ++ ys
    | xs <- moveK1 c d
    , ys <- type1 d ds
    ]

type2 :: K2 -> [K2] -> [[K2]]
type2 c = \case
  [] -> [[]]
  d:ds ->
    [ xs ++ [A] ++ ys
    | xs <- moveK2 c d
    , ys <- type2 d ds
    ]

allPosTyings :: String -> [[K2]]
allPosTyings s =
  [ zs
  | xs <- type1 'A' s
  , ys <- type2 A xs
  , zs <- type2 A ys
  ]

play :: String -> IO ()
play s = do
  print ("play",s)
  --sequence_ [ putStrLn (seeK2s ks) | ks <- allPosTyings s ]
  let _ = seeK2s
  let ns = [ length xs | xs <- allPosTyings s ]
  let i = read @Int (init s)
  let j = minimum ns
  print (length ns)
  print (j,i)



part1 :: [String] -> Int
part1 = sum . map complexity

complexity :: String -> Int
complexity s = do
  let ns = [ length xs | xs <- allPosTyings s ]
  let i = read @Int (init s)
  let j = minimum ns
  (i*j)
