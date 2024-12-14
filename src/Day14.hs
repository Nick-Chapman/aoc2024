module Day14 (main) where

import Control.Monad (forM_)
import Data.Map as Map
import Misc (check,hist)
import Par4 (parse,Par,separated,nl,lit,key,int,alts)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day14.sample"
  inp <- parse gram <$> readFile "input/day14.input"
  let boundSam = (11,7)
  let boundInp = (101,103)
  print ("day14, part1 (sample)", check 12 $ part1 boundSam sam)
  print ("day14, part1", check 221616000 $ part1 boundInp inp)
  let _ = searchPart2 10000 boundInp inp
  part2 7572 boundInp inp

searchPart2 :: Int -> Pos -> Input -> IO ()
searchPart2 max bound rs = do
  forM_ [0..max] $ \nSteps -> do
    print nSteps
    putStrLn (pic bound [ locate bound nSteps r | r <- rs ])

part2 :: Int -> Pos -> Input -> IO ()
part2 nSteps bound rs =
  putStrLn (pic bound [ locate bound nSteps r | r <- rs ])

pic :: Pos -> [Pos] -> String
pic (xB,yB) ps = do
  let h = hist ps
  let
    cell x y =
      case Map.lookup (x,y) h of
        Nothing -> "."
        Just n -> show n
  unlines [ concat [ cell x y | x <- [0..xB-1] ] | y <- [0..yB-1] ]


type Pos = (Int,Int)
type Robot = (Pos,Pos)
type Input = [Robot]

gram :: Par Input
gram = separated nl rob
  where
    rob = do key "p="; p <- pos; key " v="; v <- pos; pure (p,v)
    pos = do x <- sint; lit ','; y <- sint; pure (x,y)
    sint = alts [int , negate <$> do lit '-'; int]

part1 :: Pos -> Input -> Int
part1 bound rs = do
  let nSteps = 100
  let qs = [ locate bound nSteps r | r <- rs ]
  let (a,b,c,d) = quaterize bound qs
  a*b*c*d

quaterize :: Pos -> [Pos] -> (Int,Int,Int,Int)
quaterize (xB,yB) ps =
  let xM = xB `div` 2 in
  let yM = yB `div` 2 in
  ( length [ p | p@(x,y) <- ps, x < xM, y < yM ]
  , length [ p | p@(x,y) <- ps, x > xM, y < yM ]
  , length [ p | p@(x,y) <- ps, x < xM, y > yM ]
  , length [ p | p@(x,y) <- ps, x > xM, y > yM ]
  )

locate :: Pos -> Int -> Robot -> Pos
locate (xB,yB) n ((xI,yI),(xV,yV)) = (calc xB xI xV, calc yB yI yV)
  where
    calc b i v = (i + n * v) `mod` b
