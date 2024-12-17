module Day17 (main) where

import Misc (check)
import Par4 (parse,Par,key,int,separated,nl,lit)
import Control.Monad (ap,liftM)
import Data.Bits (xor)
import qualified Day17_part2

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day17.sample"
  inp <- parse gram <$> readFile "input/day17.input"
  print ("day17, part1 (sample)", check [4,6,3,5,6,3,5,2,1,0] $ part1 sam)
  print ("day17, part1", check [2,1,0,4,6,2,4,2,0] $ part1 inp)
  sam2 <- parse gram <$> readFile "input/day17.sample2"
  print ("day17, part2 (sample2)", check 117440 $ part2 sam2)
  Day17_part2.main

type Input = (State,Prog)

gram :: Par Input
gram = do
  key "Register A: "; a <- int; nl
  key "Register B: "; b <- int; nl
  key "Register C: "; c <- int; nl
  nl
  key "Program: "; prog <- separated (lit ',') int
  pure (State{a,b,c,ip=0},prog)

part1 :: Input -> [Int]
part1 (s,p) = execute p s emulate

part2 :: Input -> Int
part2 (s,prog) = head [ a | a <- [0..], execute prog s { a } emulate == prog ]

emulate :: M ()
emulate = do
  x <- Fetch
  dispatch (decode x)
  emulate

data Op = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
data Reg = A | B | C

decode :: Int -> Op
decode = \case
  0 -> Adv
  1 -> Bxl
  2 -> Bst
  3 -> Jnz
  4 -> Bxc
  5 -> Out
  6 -> Bdv
  7 -> Cdv
  _ -> error "decode"

dispatch :: Op -> M ()
dispatch = \case
  Adv -> divideInto A

  Bxl -> do
    v1 <- Get B
    v2 <- Fetch
    Set B (v1 `xor` v2)

  Bst -> do
    v <- combo;
    Set B (v `mod` 8)

  Jnz -> do
    a <- Get A
    n <- Fetch
    if a == 0 then pure () else Jump n

  Bxc -> do
    _ <- Fetch
    v1 <- Get B
    v2 <- Get C
    Set B (v1 `xor` v2)

  Out -> do
    v <- combo;
    Output (v `mod` 8)

  Bdv -> undefined B
  Cdv -> divideInto C

divideInto :: Reg -> M ()
divideInto target = do
  num <- Get A
  dem <- (2^) <$> combo
  Set target (num `div` dem)

combo :: M Int
combo = do
  Fetch >>= \case
    0 -> pure 0
    1 -> pure 1
    2 -> pure 2
    3 -> pure 3
    4 -> Get A
    5 -> Get B
    6 -> Get C
    7 -> error "combo,7"
    _ -> error "combo"

instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Get :: Reg -> M Int
  Set :: Reg -> Int -> M ()
  Fetch :: M Int
  Jump :: Int -> M ()
  Output :: Int -> M ()

execute :: Prog -> State -> M () -> Res
execute prog s0 m = loop m s0 undefined
  where
    max = length prog
    loop :: M a -> State -> (State -> a -> Res) -> Res
    loop m s@State{a,b,c,ip} k = case m of
      Ret x -> k s x
      Bind m f -> loop m s $ \s a -> loop (f a) s k
      Get reg -> k s (case reg of A -> a; B -> b; C -> c)
      Set reg v -> k (case reg of A -> s{a=v}; B -> s{b=v}; C -> s{c=v}) ()
      Fetch -> if ip >= max then [] else k s { ip = 1+ip } (prog!!ip)
      Jump ip -> k s { ip } ()
      Output v -> v : k s ()

type Res = [Int]

type Prog = [Int]
data State = State { a,b,c,ip :: Int } deriving Show
