module Day1 (main) where

import Misc (check)
import Data.List (sort)
import Par4 (parse,Par,separated,nl,int,ws1)

main :: IO ()
main = do
  sam <- readFile "input/day1.sample"
  inp <- readFile "input/day1.input"
  print ("day1, part1 (sample)", check 11 $ part1 sam)
  print ("day1, part1", check 1889772 $ part1 inp)
  print ("day1, part2 (sample)", check 31 $ part2 sam)
  print ("day1, part2", check 23228917 $ part2 inp)

gram :: Par [(Int,Int)]
gram = separated nl $ do
  i <- int
  ws1
  j <- int
  pure (i,j)

part1 :: String -> Int
part1 s = do
  let (ys,zs) = unzip (parse gram s)
  sum [ abs (y-z) | (y,z) <- zip (sort ys) (sort zs) ]

part2 :: String -> Int
part2 s = do
  let (ys,zs) = unzip (parse gram s)
  sum [ y * length [ () | z <- zs, z == y ]
      | y <- ys ]
