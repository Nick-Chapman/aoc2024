module Day5 (main) where

import Misc (check)
import Par4 (parse,Par,int,lit,nl,terminated,separated)
import Data.Set as Set
import Data.List as List (sortBy)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day5.sample"
  inp <- parse gram <$> readFile "input/day5.input"
  print ("day5, part1 (sample)", check 143 $ part1 sam)
  print ("day5, part1", check 6260 $ part1 inp)
  print ("day5, part2 (sample)", check 123 $ part2 sam)
  print ("day5, part2 (sample)", check 5346 $ part2 inp)

type Page = Int
type Order = (Page,Page)
type Constraints = Set Order
type Update = [Page]
type Input = (Constraints,[Update])

gram :: Par Input
gram = input
  where
    input = do
      os <- orders; nl
      us <- updates
      pure (os,us)
    orders = Set.fromList <$> terminated nl order
    order = do i <- int; lit '|'; j <- int; pure (i,j)
    updates = separated nl update
    update = separated (lit ',') int

part1 :: Input -> Int
part1 (cs,us) = sum [ middle u | u <- us, u `satisfies` cs ]

part2 :: Input -> Int
part2 (cs,us) = sum [ middle (putInOrder cs u) | u <- us, not (u `satisfies` cs) ]

putInOrder :: Constraints -> [Page] -> [Page]
putInOrder cs ps = List.sortBy f ps
  where f p q = if (p,q) `elem` cs then LT else GT

middle ::Update -> Page
middle xs = do
  let n = length xs
  if n `mod` 2 == 0 then error "even" else
    xs !! (n `div` 2)

satisfies :: Update -> Constraints -> Bool
satisfies u cs = all (\o -> opposite o `notElem` cs) (ordersOf u)

ordersOf :: Update -> [Order]
ordersOf u = [ (x,y) | x:ys <- tails u, y <- ys ]

tails :: [a] -> [[a]]
tails = \case [] -> [[]]; xs@(_:xs') -> xs : tails xs'

opposite :: Order -> Order
opposite (x,y) = (y,x)
