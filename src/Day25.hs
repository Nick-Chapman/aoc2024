module Day25 (main) where

import Data.List (transpose)
import Misc (check)
import Par4 (parse,Par,terminated,separated,nl,some,alts,lit)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day25.sample"
  inp <- parse gram <$> readFile "input/day25.input"
  print ("day25, part1 (sample)", check 3 $ part1 sam)
  print ("day25, part1", check 3090 $ part1 inp)

type Input = [Grid]
type Grid = [[Tile]]
type Tile = Bool

data Lock = Lock [Int] deriving Show
data Key = Key [Int] deriving Show

gram :: Par Input
gram = separated nl grid
  where
    grid = terminated nl line
    line = some tile
    tile = alts [ do lit '#'; pure True , do lit '.'; pure False ]

part1 :: Input -> Int
part1 gs = length [ (l,k) | l <- locks , k <- keys , fit l k]
  where
    locks = [ asLock g | g <- gs, isLock g ]
    keys = [ asKey g | g <- gs, not (isLock g) ]
    fit (Lock xs) (Key ys) = length [ () | (x,y) <- zip xs ys, x+y <= 5 ] == 5

    isLock xs = all id (head xs)
    asLock xs = Lock [ length (takeWhile id col) | col <- transpose (tail xs) ]
    asKey xs = Key [ 5 - length (takeWhile not col) | col <- transpose (tail xs) ]
