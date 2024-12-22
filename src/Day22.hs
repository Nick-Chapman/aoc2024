module Day22 (main) where

import Data.Bits (xor)
import Misc (check,collate)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse <$> readFile "input/day22.sample"
  inp <- parse <$> readFile "input/day22.input"
  print ("day22, part1 (sample)", check 37327623 $ part1 sam)
  print ("day22, part1", check 12664695565 $ part1 inp)
  let sam2 = [1,2,3,2024]
  print ("day22, part2 (sample)", check 23 $ part2 sam2)
  print ("day22, part2", check 1444 $ part2 inp)

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 is = sum [ head (drop 2000 (iterate step i)) | i <- is ]

step :: Int -> Int
step = step3 . step2 . step1
  where
    step1 s = prune (mix (s * 64) s)
    step2 s = prune (mix (s `div` 32) s)
    step3 s = prune (mix (2048 * s) s)
    mix x s = x `xor` s
    prune s = s `mod` 16777216

part2 :: [Int] -> Int
part2 is = do
  let c = collate [ (k,v) | i <- is, (k,v) <- Map.toList (genQs i) ]
  maximum [ sum vs | (_k,vs) <- c ]

type Q = (Int,Int,Int,Int)

genQs :: Int -> Map Q Int
genQs i = do
  let xs = take 2001 [ n `mod` 10 | n <- iterate step i ]
  let ds = [ (b-a) | (a,b) <- zip xs (tail xs) ]
  let ds' = tail ds
  let ds'' = tail ds'
  let ds''' = tail ds''
  let xs'''' = tail (tail (tail (tail xs)))
  let qs = [ ((a,b,c,d),e) | (a,(b,(c,(d,e)))) <-  zip ds (zip ds' (zip ds'' (zip ds''' xs''''))) ]
  Map.fromList (reverse qs)
