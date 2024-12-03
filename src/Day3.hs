module Day3 (main) where

import Misc (check)
import Par4 (parse,Par,alts,noError,key,sat,lit,digit,opt)

main :: IO ()
main = do
  sam <- readFile "input/day3.sample"
  inp <- readFile "input/day3.input"
  print ("day3, part1 (sample)", check 161 $ part1 sam)
  print ("day3, part1", check 184576302 $ part1 inp)
  sam2 <- readFile "input/day3.sample2"
  print ("day3, part2 (sample)", check 48 $ part2 sam2)
  print ("day3, part2", check 118173507 $ part2 inp)

part1 :: String -> Int
part1 s = eval (parse gram1 s)

part2 :: String -> Int
part2 s = eval (parse gram2 s)

data Instr = Mul Int Int

eval :: [Instr] -> Int
eval is = sum [ x*y | Mul x y <- is ]

gram1 :: Par [Instr]
gram1 = loop []
  where
    loop acc =
      alts [ do m <- noError mul; loop (m:acc)
           , do anyChar; loop acc
           , pure (reverse acc)
           ]

gram2 :: Par [Instr]
gram2 = enabled []
  where
    enabled acc =
      alts [ do m <- noError mul; enabled (m:acc)
           , do key "don't()"; disabled acc
           , do anyChar; enabled acc
           , pure (reverse acc)
           ]

    disabled acc =
      alts [ do key "do()"; enabled acc
           , do anyChar; disabled acc
           , pure (reverse acc)
           ]

anyChar :: Par ()
anyChar = do _ <- sat (const True); pure ()

mul :: Par Instr
mul = do
  key "mul("
  x <- arg
  lit ','
  y <- arg
  lit ')'
  pure (Mul x y)

arg :: Par Int
arg = f <$> (digit @@ opt (digit @@ opt digit))
  where
    f :: (Int, Maybe (Int, Maybe Int)) -> Int
    f = \case
      (a,Nothing) -> a
      (a,Just (b,Nothing)) -> 10*a + b
      (a,Just (b,Just c)) -> 100*a +10*b + c

(@@) :: Par a -> Par b -> Par (a,b)
(@@) p q = (,) <$> p <*> q
