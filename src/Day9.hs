module Day9 (main) where

import Misc (check)
import Par4 (parse,many,digit)

main :: IO ()
main = do
  sam <- parse (many digit) <$> readFile "input/day9.sample"
  inp <- parse (many digit) <$> readFile "input/day9.input"
  print ("day9, part1 (sample)", check 1928 $ part1 sam)
  print ("day9, part1", check 6331212425418 $ part1 inp)
  print ("day9, part2 (sample)", check 2858 $ part2 sam)
  print ("day9, part2", check 6363268339304 $ part2 inp)

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

part2 :: [Int] -> Int
part2 xs = do
  let ts0 = mkExplicit xs
  let ins = [ (i,n) | (_,i,n) <- reverse ts0 ]
  loop ts0 ins
  where
    loop :: [Trip] -> [(Int,Int)] -> Int
    loop ts = \case
      [] -> score ts
      (i,n):ins -> do
        let b = canInsert i n ts
        if not b then loop ts ins else do
          loop (insert i n (remove i ts)) ins

type Trip = (Int,Int,Int) -- (gap,id,len) (g,i,n)

score :: [Trip] -> Int
score ts = sum [ pos*id | (pos,id) <- zip [0::Int ..] (ts >>= expand2) ]

expand2 :: Trip -> [Int]
expand2 (g,i,n) = replicate g 0 ++ replicate n i

mkExplicit :: [Int] -> [Trip]
mkExplicit xs =
  [ (g,i,n) |  (i,(g,n)) <- zip [0::Int ..] (pairUp (0:xs)) ]

canInsert :: Int -> Int -> [Trip] -> Bool
canInsert i' n' ts = any (\(g,_,_) -> n'<=g) (reverse (dropWhile (\(_,i,_) -> i'/=i) (reverse ts)))

remove :: Int -> [Trip] -> [Trip]
remove i0 = \case
  [] -> undefined
  [(g1,i1,n1)] -> if i0 == i1 then [] else [(g1,i1,n1)]
  (g1,i1,n1):(g2,i2,n2):xs ->
    if i0 == i1
    then (g1+n1+g2,i2,n2) :xs
    else (g1,i1,n1) : remove i0 ((g2,i2,n2):xs)

insert :: Int -> Int -> [Trip] -> [Trip]
insert i' n' = \case
  [] -> undefined
  (g,i,n):xs ->
    if n' <= g
    then (0,i',n') : (g-n',i,n) : xs
    else (g,i,n) : insert i' n' xs

pairUp :: [Int] -> [(Int,Int)]
pairUp = \case
  [] -> []
  [_] -> undefined
  x:y:xs -> (x,y):pairUp xs
