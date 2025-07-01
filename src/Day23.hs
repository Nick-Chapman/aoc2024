module Day23 (main) where

import Data.List (maximumBy,sort,intercalate)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set ((\\))
import Misc (check,collate,nub,head)
import Par4 (parse,Par,separated,nl,word,lit)
import Prelude hiding (head)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day23.sample"
  inp <- parse gram <$> readFile "input/day23.input"
  print ("day23, part1 (sample)", check 7 $ part1 sam)
  print ("day23, part1", check 1304 $ part1 inp)
  print ("day23, part2 (sample)", check "co,de,ka,ta" $ part2 sam)
  print ("day23, part2", check "ao,es,fe,if,in,io,ky,qq,rd,rn,rv,vc,vl" $ part2 inp)

type Input = [(Id,Id)]
type Id = String

gram :: Par Input
gram = separated nl pair where
  pair = do a <- word; lit '-'; b <- word; pure (a,b)

type Con = Id -> [Id]
mkCon :: Input -> Con
mkCon xs = \k -> maybe undefined id $ Map.lookup k m
  where m :: Map Id [Id] = Map.fromList $ collate [ p | (a,b) <- xs, p <- [(a,b),(b,a)] ]

part1 :: Input -> Int
part1 xs = do
  let con = mkCon xs
  let all = nub (map fst xs ++ map snd xs)
  let startsT s = head s == 't'
  let
    trips =
      [ (a,b,c)
      | a <- all
      , b <- con a
      , a < b
      , c <- con b
      , b < c
      , a `elem` con c
      , startsT a || startsT b || startsT c
      ]
  length trips

part2 :: Input -> String
part2 xs = do
  let con = mkCon xs
  let all = nub (map fst xs ++ map snd xs)
  let ps = [ (length clique,clique)  | x <- all, let clique = findCliqueFrom con x all ]
  intercalate "," $ sort $ snd $ maximumBy (comparing fst) ps

findCliqueFrom :: Con -> Id -> [Id] -> [Id]
findCliqueFrom con x xs = loop (Set.singleton x) xs
  where
    loop acc = \case
      [] -> Set.toList acc
      y:ys ->
        if Set.null (acc \\ (Set.fromList (con y)))
        then loop (Set.insert y acc) ys
        else loop acc ys
