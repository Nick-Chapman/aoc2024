module Day16 (main) where

import Misc (check,the)
import Par4 (parse,Par,separated,nl,some,alts,lit)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.sample"
  sam2 <- parse gram <$> readFile "input/day16.sample2"
  inp <- parse gram <$> readFile "input/day16.input"

  part1_sam <- part1 sam
  part1_sam2 <- part1 sam2
  part1_inp <- part1 inp

  print ("day16, part1 (sample)", check 7036 $ part1_sam)
  print ("day16, part1 (sample2)", check 11048 $ part1_sam2)
  print ("day16, part1", check 130536 $ part1_inp)

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

type Node = (Pos,Dir)
data Dir = N | E | S | W deriving (Eq,Ord,Show)
type Cost = Int

part1 :: Input -> IO Int
part1 i = do
  let start = the [ p | (p,Start) <- Map.toList i ]
  let end = the [ p | (p,End) <- Map.toList i ]
  let init :: Node = (start,E)
  let isFinishedNode :: Node -> Bool = \(p,_) -> p==end
  let s0 = state0 [init]
  let isWall p = Map.lookup p i == Just Wall
  let
    step :: Node -> [(Node,Cost)]
    step (p,d) =
      [ ((p,d),c)
      | ((p,d),c) <- [ ((forward p d,d),1), ((p,clock d),1000), ((p,anti d),1000) ]
      , not (isWall p)
      ]
  let
    _seeState :: State -> String
    _seeState State {v,q} = do
      let hq = case q of [] -> "[]"; x:_ -> show x
      show ("cost_", hq, "#nodes=", Set.size v)

  let waves = search step s0
  let isFinishedState State{q} = let (_,n) = head q in isFinishedNode n
  let (_pre,post) = span (not . isFinishedState) waves

  --mapM_ (putStrLn . _seeState) _pre

  let finishedState = head post
  let State{q=finalQ} = finishedState
  let (finalCost,_) = head finalQ
  pure finalCost

clock :: Dir -> Dir
clock = \case N -> E; E -> S; S -> W; W -> N

anti :: Dir -> Dir
anti = \case N -> W; E -> N; S -> E; W -> S

forward :: Pos -> Dir -> Pos
forward (x,y) = \case
  N -> (x,y-1)
  E -> (x+1,y)
  S -> (x,y+1)
  W -> (x-1,y)

-----------------------------------------------------------------------
-- copied code

data State = State { v :: Set Node, q :: Q } deriving Show
type Q = [ (Cost,Node) ]

pushQ :: Q -> (Cost,Node) -> Q
pushQ q (c,n) =
  case q of
    [] -> [(c,n)]
    x@(c1,n1):q' ->
      if c <= c1 then (c,n):q else
        if n==n1 then q else
          x : pushQ q' (c,n)

state0 :: [Node] -> State
state0 ns = State { v = Set.empty, q = [ (0, n) | n <- ns ] }

search :: (Node -> [(Node,Cost)]) -> State -> [State]
search step = loop
  where
    loop :: State -> [State]
    loop s@State{v,q} = s :
      case q of
        [] -> []
        (c1,nodeSelected):q -> do
          if nodeSelected `Set.member` v then loop s { q } else do
            let q' = foldl pushQ q
                  [ ( c1+c2, n')
                  | (n',c2) <- step nodeSelected
                  , not (n' `Set.member` v)
                  ]
            loop State { v = Set.insert nodeSelected v, q = q' }
