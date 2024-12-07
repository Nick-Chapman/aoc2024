module Day7 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,int,key,ws1)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day7.sample"
  inp <- parse gram <$> readFile "input/day7.input"
  print ("day7, part1 (sample)", check 3749 $ part1 sam)
  print ("day7, part1", check 1620690235709 $ part1 inp)
  print ("day7, part2 (sample)", check 11387 $ part2 sam)
  print ("day7, part2", check 145397611075341 $ part2 inp)
  where
    part1 = test [ (+), (*) ]
    part2 = test [ (+), (*), extraOp ]

data Line = Line Int [Int] deriving Show
type Op = Int -> Int -> Int

gram :: Par [Line]
gram = separated nl line
  where line = do res <- int; key ": "; xs <- separated ws1 int; pure (Line res xs)

test :: [Op] -> [Line] -> Int
test ops lines = sum [ res | Line res (x:xs) <- lines, res `elem` comps x xs ]
  where
    comps :: Int -> [Int] -> [Int]
    comps acc = \case
      [] -> [acc]
      x:xs -> [ v | op <- ops, v <- comps (acc `op` x) xs ]

extraOp :: Int -> Int -> Int
extraOp a b = (10 ^ (log10 b)) * a + b

log10 :: Int -> Int
log10 x =
  if x < 10 then 1 else
  if x < 100 then 2 else
  if x < 1000 then 3 else
    error (show x)
