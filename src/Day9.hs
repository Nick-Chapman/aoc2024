module Day9 (main) where

import Misc (check)
import Par4 (parse,many,digit)

main :: IO ()
main = do
  sam <- parse (many digit) <$> readFile "input/day9.sample"
  inp <- parse (many digit) <$> readFile "input/day9.input"
  print ("day9, part1 (sample)", check 1928 $ part1 sam)
  print ("day9, part1", check 6331212425418 $ part1 inp)

part1 :: [Int] -> Int
part1 xs = do
  let f = expand xs
  let b = [ x | Just x <- reverse f ]
  sum [ pos*id | (pos,id) <- zip [0::Int ..] (take (length b) (merge f b)) ]

expand :: [Int] -> [Maybe Int]
expand = loop 0
  where
    loop :: Int -> [Int] -> [Maybe Int]
    loop id = \case
      [] -> []
      [i] -> replicate i (Just id)
      i:j:xs -> replicate i (Just id) ++ replicate j Nothing ++ loop (id+1) xs

merge :: [Maybe Int] -> [Int] -> [Int]
merge ms ds = case (ms,ds) of
  (Just d:ms,ds) -> d : merge ms ds
  (Nothing:ms,d:ds) -> d : merge ms ds
  (Nothing:ms,[]) -> undefined ms
  ([],ds) -> ds
