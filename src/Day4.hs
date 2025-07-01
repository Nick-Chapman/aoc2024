module Day4 (main) where

import Misc (check,head,tail)
import Prelude hiding (head,tail)

main :: IO ()
main = do
  sam <- lines <$> readFile "input/day4.sample"
  inp <- lines <$> readFile "input/day4.input"
  print ("day4, part1 (sample)", check 18 $ part1 sam)
  print ("day4, part1", check 2434 $ part1 inp)
  print ("day4, part2 (sample)", check 9 $ part2 sam)
  print ("day4, part1", check 1835 $ part2 inp)

type Grid = [String]

part1 :: Grid -> Int
part1 g0 = do
  let key = "XMAS"
  sum [ countOccs key line | tr <- trans8, line <- tr g0 ]

countOccs :: String -> String -> Int
countOccs k s =
  length [ () | s1 <- tails s, matchPrefix k s1 ]

tails :: [a] -> [[a]]
tails = \case
  [] -> [[]]
  xs@(_:xs') -> xs : tails xs'

matchPrefix :: Eq a => [a] -> [a] -> Bool
matchPrefix k s = do
  let s1 = take (length k) s
  s1 == k

trans8 ::[ [[a]] -> [[a]] ]
trans8 =
  [ id
  , t90
  , t45
  , t45 . t180
  , t180
  , t180 . t90
  , t180 . t45
  , t180 . t45 . t180
  ]

part2 :: Grid -> Int
part2 g0 = do
  sum [ if matchXmasCross g then 1 else 0 | tr <- trans4, let g1 = tr g0, g2 <- rights g1, g <- downs g2 ]

matchXmasCross :: Grid -> Bool
matchXmasCross = \case
  ('M':_:'S':_) : (_:'A':_) : ('M':_:'S':_) :_ -> True
  _ -> False

rights :: Grid -> [Grid]
rights = unfold (\g -> (g, map tail g)) stop
  where stop = isEmpty . head

downs :: Grid -> [Grid]
downs = unfold (\g -> (g, tail g)) stop
  where stop = isEmpty

trans4 ::[ [[a]] -> [[a]] ]
trans4 =
  [ id
  , t90
  , t180
  , t180 . t90
  ]

t180 :: [[a]] -> [[a]]
t180 = map reverse

t90 :: forall a. [[a]] -> [[a]]
t90 xss = unfold u isEmpty xss
  where
    u :: [[a]] -> ([a], [[a]])
    u xss = mapFst dropNone $ unzip [ u1 xs | xs <- xss ]

    u1 :: [a] -> (Maybe a, [a])
    u1 = \case
      [] -> (Nothing,[])
      x:xs -> (Just x, xs)

t45 :: forall a. [[a]] -> [[a]]
t45 xss = unfold u isEmpty staggered
  where
    staggered :: [(Int,[a])]
    staggered = zip [0..] xss

    u :: [(Int,[a])] -> ([a], [(Int,[a])])
    u xss = mapFst dropNone $ unzip [ u1 xs | xs <- xss ]

    u1 :: (Int,[a]) -> (Maybe a, (Int,[a]))
    u1 = \case
      (0,[]) -> (Nothing,(0,[]))
      (0,x:xs) -> (Just x, (0,xs))
      (n,xs) -> (Nothing, (n-1,xs))


unfold :: (a -> (b,a)) -> (b -> Bool) -> a -> [b]
unfold f p a = do
  let (b,a') = f a
  if p b then [] else b : unfold f p a'

isEmpty :: [a] -> Bool
isEmpty = \case
  [] -> True
  _:_ -> False

dropNone :: [Maybe a] -> [a]
dropNone ms = [ a | Just a <- ms ]

mapFst :: (a->c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a,b)
