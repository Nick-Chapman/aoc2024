module Day16 (main) where

import Misc (check,the,collate)
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
  print ("day16, part1 (sample)", check 7036 $ part1_sam)

  part1_sam2 <- part1 sam2
  print ("day16, part1 (sample2)", check 11048 $ part1_sam2)

  part1_inp <- part1 inp
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
  let s0 = state0 [(0,init)]
  let waves = search (step i) s0
  let isFinishedState State{q} = let (_,n) = headQ q in isFinishedNode n
  let (_pre,post) = span (not . isFinishedState) waves
  --mapM_ print (zip [0::Int ..] _pre)
  let finishedState = head post
  let State{q=finalQ} = finishedState
  --print ("head(finalQ):",headQ finalQ)
  let (finalCost,_) = headQ finalQ
  pure finalCost


step :: Input -> Node -> [(Node,Cost)]
step i (p,d) =
  (let p1 = forward p d in if isWall p1 then [] else [((p1,d),1)]) ++
  (let d1 = anti d in let p1 = forward p d1 in if isWall p1 then [] else [((p1,d1),1001)]) ++
  (let d1 = clock d in let p1 = forward p d1 in if isWall p1 then [] else [((p1,d1),1001)])
  where isWall p = Map.lookup p i == Just Wall

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

instance Show State where
  show State {v,q} = do
    show ("headQ=",headQ q, "lenQ=",sizeQ q, "#visited=",Set.size v)


headQ :: Q -> (Cost,Node)
headQ q = case unQ q of
  Nothing -> error "headQ"
  Just (c,n,_) -> (c,n)

-----------------------------------------------------------------------
-- copied code

data State = State { v :: Set Node, q :: Q }

state0 :: [(Cost,Node)] -> State
state0 xs = State { v = Set.empty, q = initQ xs }

search :: (Node -> [(Node,Cost)]) -> State -> [State]
search step = loop
  where
    loop :: State -> [State]
    loop s@State{v,q} = s :
      case unQ q of
        Nothing -> []
        Just (c1,nodeSelected,q) -> do
          --if nodeSelected `Set.member` v then loop s { q } else do
            let q' = foldl pushQ q
                  [ ( c1+c2, n')
                  | (n',c2) <- step nodeSelected
                  , not (n' `Set.member` v)
                  ]
            loop State { v =
                         Set.insert nodeSelected
                         v
                       , q = q'
                       }

----------------------------------------------------------------------
{-data Q

sizeQ :: Q -> Int
sizeQ = undefined

initQ :: [(Cost,Node)] -> Q
initQ = undefined

pushQ :: Q -> (Cost,Node) -> Q
pushQ = undefined

unQ :: Q -> Maybe (Cost,Node,Q)
unQ = undefined
-}

----------------------------------------------------------------------

{-
type Q = [ (Cost,Node) ]

sizeQ :: Q -> Int
sizeQ = length

initQ :: [(Cost,Node)] -> Q
initQ = id

pushQ :: Q -> (Cost,Node) -> Q
pushQ q (c,n) =
  case q of
    [] -> [(c,n)]
    x@(c1,n1):q' ->
      if c <= c1 then (c,n):q else
        if n==n1 then q else
          x : pushQ q' (c,n)

unQ :: Q -> Maybe (Cost,Node,Q)
unQ = \case [] -> Nothing; (cost,nodeSelected):q -> Just (cost,nodeSelected,q)
-}
----------------------------------------------------------------------

type Q = [(Cost,Set Node)]

sizeQ :: Q -> Int
sizeQ = length

initQ :: [(Cost,Node)] -> Q
initQ cns = [ (c,Set.fromList ns) | (c,ns) <- collate cns ]

unQ :: Q -> Maybe (Cost,Node,Q)
unQ xs = case xs of
  [] -> Nothing
  (c,ns):xs ->
    case Set.minView ns of
      Nothing -> unQ xs
      Just (n,ns) -> Just (c,n, ((c,ns):xs))

pushQ :: Q -> (Cost,Node) -> Q
pushQ xs (c,n) =
  case xs of
    [] -> [(c,Set.singleton n)]
    x@(c1,ns1):xs' ->
      if c < c1 then (c,Set.singleton n):xs else
        if c == c1 then (c,Set.insert n ns1):xs' else
          x : pushQ xs' (c,n)



