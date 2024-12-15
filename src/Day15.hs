module Day15 (main) where

import Data.Map as Map
import Misc (check,the)
import Par4 (parse,Par,separated,terminated,many,some,nl,lit,alts)

main :: IO ()
main = do
  sam1 <- parse gram <$> readFile "input/day15.sample1"
  sam2 <- parse gram <$> readFile "input/day15.sample2"
  inp <- parse gram <$> readFile "input/day15.input"
  part1_sam1 <- part1 sam1
  part1_sam2 <- part1 sam2
  part1_inp <- part1 inp
  print ("day15, part1 (sample1)", check 2028 $ part1_sam1)
  print ("day15, part1 (sample2)", check 10092 $ part1_sam2)
  print ("day15, part1", check 1492518 $ part1_inp)

type Input = (Pos, World, Prog)
type World = Map Pos Tile
data Tile = Gap | Box | Wall | Robot
type Prog = [Dir]
data Dir = N | E | S | W deriving Show
type Pos = (Int,Int)

gram :: Par Input
gram = do m <- map; nl; p <- prog; pure (bounds m,makeWorld m,p)
  where
    bounds tss = (length (head tss), length tss)
    map = terminated nl (some tile)
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

part1 :: Input -> IO Int
part1 (_max,w0,prog) = do
  let r0 = locateRobot w0
  let state0 = (r0,w0)
  w <- loop 0 state0 prog
  pure (score w)
  where
    loop :: Int -> State -> Prog -> IO World
    loop i state@(_r,w) prog = do
      case prog of
        [] -> pure w
        dir:prog -> do
          let state' = step dir state
          loop (i+1) state' prog

locateRobot :: World -> Pos
locateRobot w = the [ p | (p,Robot) <- Map.toList w ]

score :: World -> Int
score w = sum [ 100*y+x | ((x,y),Box) <- Map.toList w ]

type State = (Pos,World)

step :: Dir -> State -> State
step d (p,w) = do
  let n = shiftLength p w d
  if n == 0 then (p,w) else do
    let w1 = shunt n p w d
    let w2 = Map.insert p Gap w1
    let p' = adj p d
    (p',w2)

shunt :: Int -> Pos -> World -> Dir -> World
shunt n p w d =
  if n == 0 then w else
    move1 p d (shunt (n-1) (adj p d) w d)

move1 :: Pos -> Dir -> World -> World
move1 p d w = do
  let t = maybe undefined id $ Map.lookup p w
  let p' = adj p d
  Map.insert p' t w

shiftLength :: Pos -> World -> Dir -> Int
shiftLength p w d = do
  let col = infColumn w d p
  let isMovable = \case Box -> True; Robot -> True; Wall -> False; Gap -> False
  let (h,t) = span isMovable col
  let canMove = case (head t) of Box -> undefined; Robot -> undefined; Wall -> False; Gap -> True
  let n = if canMove then length h else 0
  n

infColumn :: World -> Dir -> Pos -> [Tile]
infColumn w d p = [ look p | p <- adjs d p ]
  where look p = maybe undefined id $ Map.lookup p w

adjs :: Dir -> Pos -> [Pos]
adjs d p = p : adjs d (adj p d)

adj :: Pos -> Dir -> Pos
adj (x,y) = \case
  N -> (x,y-1)
  E -> (x+1,y)
  S -> (x,y+1)
  W -> (x-1,y)
