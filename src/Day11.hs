module Day11 (main) where

import Misc (check,collate)
import Par4 (parse,Par,separated,ws1,int)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day11.sample"
  inp <- parse gram <$> readFile "input/day11.input"
  print ("day11, part1 (sample)", check 55312 $ part1 sam)
  print ("day11, part1", check 233050 $ part1 inp)
  print ("day11, part2", check 276661131175807 $ part2 inp)
    where
      part1 = go 25
      part2 = go 75

type Input = [Stone]
type Stone = Int

gram :: Par Input
gram = separated ws1 int

go :: Int -> Input -> Int
go n i = do
  let s0 = initState i
  let f = composeN n stepState
  let res = f s0
  lengthState res

composeN :: Int -> (a -> a) -> (a -> a)
composeN n f =
  if n == 0 then undefined else
    if n == 1 then f else do
      let ff = f . f
      if n `mod` 2 == 0
        then composeN (n `div` 2) ff
        else composeN (n `div` 2) ff . f

type State = Map Stone Int -- freq of each kind of stone

lengthState :: State -> Int
lengthState m = sum [ freq | freq <- Map.elems m ]

initState :: Input -> State
initState xs = Map.fromList [ (x,1) | x <- xs ]

stepState :: State -> State
stepState m = Map.fromList
  [ (x, sum freqs)
  | (x,freqs) <- collate [ (x',freq) | (x,freq) <- Map.toList m
                                     , x' <- step x
                                     ]
  ]

step :: Stone -> [Stone]
step = \case
  0 -> [1]
  stone ->
    case maybeHalfDigits stone of
      Nothing -> [stone*2024]
      Just m -> [stone `div` m, stone `mod` m]

maybeHalfDigits :: Stone -> Maybe Int
maybeHalfDigits s =
  if s < 10 then Nothing else
  if s < 100 then Just 10 else
  if s < 1000 then Nothing else
  if s < 10000 then Just 100 else
  if s < 100000 then Nothing else
  if s < 1000000 then Just 1000 else
  if s < 10000000 then Nothing else
  if s < 100000000 then Just 10000 else
  if s < 1000000000 then Nothing else
  if s < 10000000000 then Just 100000 else
  if s < 100000000000 then Nothing else
  if s < 1000000000000 then Just 1000000 else
  error (show ("maybeHalfDigits",s))
