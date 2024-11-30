module Day1 (main) where

import Misc (check)

main :: IO ()
main = do
  sam <- readFile "input/day1.sample"
  inp <- readFile "input/day1.input"
  print ("day1, part1 (sample)", check 0 $ part1 sam)
  print ("day1, part1", check 0 $ part1 inp)

part1 :: String -> Int
part1 = undefined
