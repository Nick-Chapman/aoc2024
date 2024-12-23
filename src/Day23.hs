module Day23 (main) where

import Data.Map (Map)
import Misc (check,collate,nub)
import Par4 (parse,Par,separated,nl,word,lit)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day23.sample"
  inp <- parse gram <$> readFile "input/day23.input"
  print ("day23, part1 (sample)", check 7 $ part1 sam)
  print ("day23, part1", check 1304 $ part1 inp)

type Input = [(Id,Id)]
type Id = String

gram :: Par Input
gram = separated nl pair where
  pair = do a <- word; lit '-'; b <- word; pure (a,b)

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

type Con = Id -> [Id]
mkCon :: Input -> Con
mkCon xs = \k -> maybe undefined id $ Map.lookup k m
  where m :: Map Id [Id] = Map.fromList $ collate [ p | (a,b) <- xs, p <- [(a,b),(b,a)] ]
