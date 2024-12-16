module Day13 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,key,int,)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day13.sample"
  inp <- parse gram <$> readFile "input/day13.input"
  print ("day13, part1 (sample)", check 480 $ go sam)
  print ("day13, part1", check 32026 $ go inp)
  print ("day13, part2", check 89013607072065 $ go (map correct inp))

correct :: Puz -> Puz
correct (a,b,c,d,e,f) = (a,b,c,d,e+big,f+big) where big = 10000000000000

type Puz = (Int,Int,Int,Int,Int,Int)

gram :: Par [Puz]
gram = separated (do nl;nl) puz
  where
    puz = do
      key "Button A: X+"; a <- int; key ", Y+"; c <- int; nl
      key "Button B: X+"; b <- int; key ", Y+"; d <- int; nl
      key "Prize: X="; e <- int; key ", Y="; f <- int
      pure (a,b,c,d,e,f)

go :: [Puz] -> Int
go = sum . map solve

solve :: Puz -> Int
solve (a,b,c,d,e,f) = do
  let det = a*d - b*c
  if (det == 0) then error "det0" else do
    let idet = d*e + (-b)*f
    let jdet = (-c)*e + a*f
    if (idet `mod` det == 0) && (jdet `mod` det == 0)
    then 3* (idet `div` det) + jdet `div` det
    else 0
