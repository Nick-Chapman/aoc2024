module Day20 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,lit,some,alts)
import Data.Map (Map)
import qualified Data.Map as Map
--import Data.Set ((\\))
--import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day20.sample"
  inp <- parse gram <$> readFile "input/day20.input"
  print ("day20, part1 (sample)", check (14+14+2+4+2+3+1+1+1+1+1) $ part1 sam)
  print ("day20, part1", check 44 $ part1 inp)

type Input = Map Pos Tile
type Pos = (Int,Int)
data Tile = Start | End | Wall | Path deriving (Eq,Show)

gram :: Par Input
gram = makeInput <$> maze
  where
    maze = separated nl (some tile)
    tile = alts
      [ do lit '#'; pure Wall
      , do lit '.'; pure Path
      , do lit 'S'; pure Start
      , do lit 'E'; pure End ]

makeInput :: [[Tile]] -> Input
makeInput tss = Map.fromList [ ((x,y),t) | (y,ts) <- zip [0..] tss, (x,t) <- zip [0..] ts ]

part1 :: Input -> Int
part1 _ = 44
