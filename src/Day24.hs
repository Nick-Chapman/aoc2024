module Day24 (main) where

import Data.Map (Map)
import Misc (check)
import Par4 (parse,Par,terminated,separated,nl,some,sat,lit,key,alts)
import qualified Data.Map as Map
import Data.List (sort)
import qualified Data.Char as Char
import Data.Bits (xor)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day24.sample"
  inp <- parse gram <$> readFile "input/day24.input"
  print ("day24, part1 (sample)", check 2024 $ part1 sam)
  print ("day24, part1", check 42410633905894 $ part1 inp)

type Input = Map Name Driver
data Driver = C0 | C1 | And Name Name | Or Name Name | Xor Name Name deriving Show
type Name = String

gram :: Par Input
gram = do
  xs <- terminated nl inp
  nl
  ys <- separated nl gate
  pure $ Map.fromList (xs++ys)
  where
    inp :: Par (Name,Driver)
    inp = do
      n <- name
      key ": "
      d <- alts [ do lit '0'; pure C0; , do lit '1'; pure C1 ]
      pure (n,d)
    gate :: Par (Name,Driver)
    gate = do
      a <- name
      lit ' '
      f <- alts [ do key "AND"; pure And
                , do key "OR"; pure Or
                , do key "XOR"; pure Xor ]
      lit ' '
      b <- name
      key " -> "
      c <- name
      pure (c, f a b)

    name = some $ sat (\c -> Char.isAlpha c || Char.isDigit c)


part1 :: Input -> Int
part1 input = do
  let znames = reverse (sort (filter startsWithZ (Map.keys input)))
        where startsWithZ s = head s == 'z'
  conv 0 (map eval znames)
  where
    eval :: Name -> Bool
    eval n =
      case look n of
        C0 -> False
        C1 -> True
        And x y -> eval x && eval y
        Or x y -> eval x || eval y
        Xor x y -> eval x `xor` eval y

    look :: Name -> Driver
    look n = maybe undefined id $ Map.lookup n input

conv :: Int -> [Bool] -> Int
conv acc = \case [] -> acc; b:bs -> conv (2*acc + (if b then 1 else 0)) bs
