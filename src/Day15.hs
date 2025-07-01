module Day15 (main) where

import Data.Map as Map
import Misc (check,the,head)
import Par4 (parse,Par,separated,terminated,many,some,nl,lit,alts)
import Prelude hiding (head)

main :: IO ()
main = do
  sam <- readFile "input/day15.sample"
  inp <- readFile "input/day15.input"
  print ("day15, part1 (sample)", check 10092 $ part1 sam)
  print ("day15, part1", check 1492518 $ part1 inp)
  print ("day15, part2 (sample)", check 9021 $ part2 sam)
  print ("day15, part2", check 1512860 $ part2 inp)
    where
      part1 = go expand1
      part2 = go expand2

type Input = (Pos, World, Prog)
type World = Map Pos Tile
data Tile = Gap | Box | Wall | Robot | BoxL | BoxR
type Prog = [Dir]
data Dir = N | E | S | W
type Pos = (Int,Int)

gram :: X -> Par Input
gram expand = do m <- map; nl; p <- prog; pure (bounds m,makeWorld m,p)
  where
    expandLine xs = [ y | x <- xs, y <- expand x ]
    bounds tss = (length (head tss), length tss)
    map = terminated nl (expandLine <$> some tile)
    prog = concat <$> separated nl (many dir)
    tile = alts
      [ do lit '#'; pure Wall
      , do lit 'O'; pure Box
      , do lit '@'; pure Robot
      , do lit '.'; pure Gap ]
    dir = alts
      [ do lit '^'; pure N
      , do lit '>'; pure E
      , do lit 'v'; pure S
      , do lit '<'; pure W ]

makeWorld :: [[Tile]] -> World
makeWorld tss = Map.fromList [ ((x,y),t) | (y,ts) <- zip [0..] tss, (x,t) <- zip [0..] ts ]

type X = Tile -> [Tile]

expand1 :: X
expand1 t = [t]

expand2 :: X
expand2 = \case
  Robot -> [Robot,Gap]
  Wall -> [Wall,Wall]
  Gap -> [Gap,Gap]
  Box -> [BoxL,BoxR]
  BoxL -> error "expand2,BoxL"
  BoxR -> error "expand2,BoxR"

go :: X -> String -> Int
go expand s = do
  let (_max,w0,prog) = parse (gram expand) s
  let r0 = locateRobot w0
  let state0 = (r0,w0)
  let w = loop 0 state0 prog
  (score w)
  where
    loop :: Int -> State -> Prog -> World
    loop i state@(_r,w) prog = do
      case prog of
        [] -> w
        dir:prog -> do
          let state' = step dir state
          loop (i+1) state' prog

locateRobot :: World -> Pos
locateRobot w = the [ p | (p,Robot) <- Map.toList w ]

score :: World -> Int
score w = sum [ 100*y+x | ((x,y),tile) <- Map.toList w, isBox tile ]

isBox :: Tile -> Bool
isBox = \case
  Box -> True
  BoxL -> True
  _ -> False

type State = (Pos,World)

step :: Dir -> State -> State
step d (p,w) = do
  case shunt w p d of
    Nothing -> (p,w)
    Just w -> (adj p d, Map.insert p Gap w)

shunt :: World -> Pos -> Dir -> Maybe World
shunt w p d = do
  case clear w (adj p d) d of
    Nothing -> Nothing
    Just w -> Just $ Map.insert (adj p d) (look w p) $ Map.insert p Gap w

clear :: World -> Pos -> Dir -> Maybe World
clear w p d =
  case look w p of
    Robot -> error "clear,Robot"
    Wall -> Nothing
    Gap -> Just w
    Box -> shunt1
    BoxL -> case d of E -> shunt1; W -> shunt1; N -> shunt2 E; S -> shunt2 E
    BoxR -> case d of E -> shunt1; W -> shunt1; N -> shunt2 W; S -> shunt2 W
  where
    shunt1 = shunt w p d
    shunt2 side = case shunt w p d of Nothing -> Nothing; Just w -> shunt w (adj p side) d

look :: World -> Pos -> Tile
look w p = maybe err id $ Map.lookup p w
  where err = error (show ("look",p))

adj :: Pos -> Dir -> Pos
adj (x,y) = \case
  N -> (x,y-1)
  E -> (x+1,y)
  S -> (x,y+1)
  W -> (x-1,y)
