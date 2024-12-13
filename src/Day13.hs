module Day13 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,key,int,)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day13.sample"
  inp <- parse gram <$> readFile "input/day13.input"
  print ("day13, part1 (sample)", check 480 $ part1 sam)
  print ("day13, part1", check 32026 $ part1 inp)

type Input = [Puz]
type Puz = (Int,Int,Int,Int,Int,Int)

gram :: Par Input
gram = separated (do nl;nl) puz
  where
    puz = do
      key "Button A: X+"
      a <- int
      key ", Y+"
      b <- int
      nl
      key "Button B: X+"
      c <- int
      key ", Y+"
      d <- int
      nl
      key "Prize: X="
      e <- int
      key ", Y="
      f <- int
      pure (a,b,c,d,e,f)

part1 :: Input -> Int
part1 = sum . map solve

solve :: Puz -> Int
solve (a,b,c,d,e,f) = do
  let
    xs =
      [ 3*i + j
      | i <- [0..100]
      , j <- [0..100]
      , i*a + j*c == e
      , i*b + j*d == f
      ]
  case xs of [] -> 0; _ -> minimum xs
