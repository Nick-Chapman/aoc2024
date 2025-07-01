module Day17_part2 (main) where

import Control.Monad (ap,liftM)
import Data.Bits (xor,shiftR)
import Data.Map (Map)
import Misc (check,head,tail)
import Par4 (parse,Par,key,int,separated,nl,lit)
import Prelude hiding (head,tail)
import Text.Printf (printf)
import qualified Data.Map as Map

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day17.input"
  part2_inp <- part2 16 inp
  print ("day17, part2", check 109685330781408 $ part2_inp)

gram :: Par Prog
gram = do
  key "Register A: "; _a <- int; nl
  key "Register B: "; _b <- int; nl
  key "Register C: "; _c <- int; nl
  nl
  key "Program: "; prog <- separated (lit ',') w3
  pure prog
  where w3 = makeW3 <$> int

part2 :: Int -> Prog -> IO Int
part2 xo prog = do
  csm :: Maybe [Constraint] <- execute xo prog emulate
  case csm of
    Nothing -> error "impossible"
    Just cs -> do
      let qs = solve 0 Map.empty (tail cs)
      let e = E_list [ E3_Var (Var n) | n <- [0..15] ]
      let r = minimum [ evalE q e | q <- qs ]
      pure r

solve :: Int -> Env -> [Constraint] -> [Env]
solve i q = \case
  [] -> [q]
  con:cons ->
    [ q2
    | v <- solve1 q (Var i) con
    , let q1 = Map.insert (Var i) v q
    , q2 <- solve (i+1) q1 cons
    ]

solve1 :: Env -> Var -> Constraint -> [W3]
solve1 q0 var c =
  [ n
  | n::W3 <- [0,1,2,3,4,5,6,7]
  , let q = Map.insert var n q0
  , evalCon q c
  ]

emulate :: M ()
emulate = do
  x <- Fetch
  dispatch (decode x)
  emulate

dispatch :: Op -> M ()
dispatch = \case
  Adv -> divideInto A

  Bxl -> do
    v1 <- Get B
    v2 <- Fetch
    Set B (v1 `xorE` (eOfW3 v2))

  Bst -> do
    v <- combo;
    Set B (eOfE3 (modE8 v))

  Jnz -> do
    a <- Get A
    n <- Fetch
    IfZero a (pure ()) (Jump n)

  Bxc -> do
    _ <- Fetch
    v1 <- Get B
    v2 <- Get C
    Set B (v1 `xorE` v2)

  Out -> do
    v <- combo;
    Output (modE8 v)

  Bdv -> undefined B
  Cdv -> divideInto C

divideInto :: Reg -> M ()
divideInto target = do
  num <- Get A
  places <- combo
  let res = num `shiftE` places
  Set target res

combo :: M E
combo = do
  W3 n <- Fetch
  case n of
    0 -> pure (eOfW3 0)
    1 -> pure (eOfW3 1)
    2 -> pure (eOfW3 2)
    3 -> pure (eOfW3 3)
    4 -> Get A
    5 -> Get B
    6 -> Get C
    7 -> error "combo,7"
    _ -> error "combo"

decode :: W3 -> Op
decode (W3 n) = case n of
  0 -> Adv
  1 -> Bxl
  2 -> Bst
  3 -> Jnz
  4 -> Bxc
  5 -> Out
  6 -> Bdv
  7 -> Cdv
  _ -> error "decode"

data Op = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
data Reg = A | B | C

instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Get :: Reg -> M E
  Set :: Reg -> E -> M ()
  Fetch :: M W3
  Jump :: W3 -> M ()
  Output :: E3 -> M ()
  IfZero :: E -> M a -> M a -> M a

type Prog = [W3]

data State = State
  { u :: Var, a,b,c :: E, ip :: Int
  , expected :: [W3]
  , constraints :: [Constraint]
  } deriving Show

execute :: Int -> Prog -> M () -> IO Res
execute xo prog m = loop m s0 f0 (error "kFinal")
  where
    f0 = undefined
    a = E_list [ E3_Var (Var n) | n <- [0..xo-1]]
    s0 = State { u = Var xo
               , a
               , b = eOfW3 0
               , c = eOfW3 0
               , ip = 0
               , expected = prog
               , constraints = []
               }
    max = length prog
    loop :: M a -> State -> IO Res -> (IO Res -> State -> a -> IO Res) -> IO Res
    loop m s@State{constraints=constraints0,expected,a,b,c,ip} fail k = case m of
      Ret x -> k fail s x
      Bind m g -> loop m s fail $ \fail s a -> loop (g a) s fail k
      Get reg -> k fail s (case reg of A -> a; B -> b; C -> c)
      Set reg v -> k fail (case reg of A -> s{a=v}; B -> s{b=v}; C -> s{c=v}) ()
      Fetch -> do
        if ip < max
          then
          do
            --print ("Fetch",prog!!ip)
            k fail s { ip = 1+ip } (prog!!ip)
          else
          do
            case expected of
              [] -> do
                --print "canHalt? - YES"
                pure (Just constraints0)
              _:_ -> do
                --print "canHalt? - NO"
                fail

      Jump (W3 ip) -> k fail s { ip } ()
      Output e -> do
        case expected of
          [] -> do
            print ("output-unexpected",e)
            undefined -- fail!
          x:expected -> do
            let con = ConOut e x
            let s' = s { expected, constraints = con:constraints0 }
            k fail s' ()

      IfZero cond t e -> do
        let
          fail1 = do
            --let con = ConNotZero cond
            ---let s' = s { constraints = con:constraints0 }
            loop e s fail k
        let con = ConZero cond
        let s' = s { constraints = con:constraints0 }
        loop t s' fail1 k


type Res = Maybe [Constraint]

eOfW3 :: W3 -> E
eOfW3 = eOfE3 . e3OfW3

eOfE3 :: E3 -> E
eOfE3 = E_Small

e3OfW3 :: W3 -> E3
e3OfW3 = E3_Lit

shiftE :: E -> E -> E
shiftE x y =
  case (x,y) of
    (E_list xs, (E_Small (E3_Lit 3))) -> E_list ((reverse . tail . reverse) xs)
    _ -> E_Shift x y

xorE :: E -> E -> E
xorE = E_Xor

modE8 :: E -> E3
modE8 = \case
  E_list [] -> E3_Lit 0
  E_list xs -> head (reverse xs)
  e -> E3_Mod8 e

newtype W3 = W3 Int deriving (Num,Eq)
makeW3 :: Int -> W3
makeW3 n = if n >= 0 && n <=7 then W3 n else error "makeW3"

newtype Var = Var Int deriving (Eq,Ord)

instance Show Constraint where
  show = \case
    ConOut e w -> printf "%s == %s" (show e) (show w)
    ConZero e -> printf "%s == 0" (show e)
    ConNotZero e -> printf "%s != 0" (show e)

instance Show E where
  show = \case
    E_Var v -> undefined v --show v
    E_Small e3 -> show e3
    E_Cons x y -> undefined x y -- printf "(%s / %s)" (show x) (show y)
    E_Shift x y -> printf "(%s >> %s)" (show x) (show y)
    E_Xor x y -> printf "(%s ^ %s)" (show x) (show y)
    E_list xs ->
      show xs

instance Show E3 where
  show = \case
    E3_Var v -> show v
    E3_Lit x -> show x
    E3_Mod8 x -> printf "(%s %% 8)" (show x)

instance Show W3 where
  show (W3 n) = printf "%d" n

instance Show Var where
  show (Var n) = printf "v%d" n

data Constraint
  = ConOut E3 W3
  | ConZero E
  | ConNotZero E

data E
  = E_Var Var
  | E_Small E3
  | E_Cons E3 E
  | E_Shift E E
  | E_Xor E E
  | E_list [E3]

data E3
  = E3_Var Var
  | E3_Lit W3
  | E3_Mod8 E


type Env = Map Var W3

evalCon :: Env -> Constraint -> Bool
evalCon q = \case
  ConOut e3 w3 ->  evalE3 q e3 == w3
  ConZero e -> evalE q e == 0
  ConNotZero e -> not (evalE q e == 0)

evalE :: Env -> E -> Int
evalE q = \case
  E_list xs -> eList 0 [ evalE3 q x | x <- xs ]
  E_Var{} -> undefined
  E_Small e3 -> n where W3 n = evalE3 q e3
  E_Cons{} -> undefined
  E_Shift a b -> evalE q a `shiftR` evalE q b
  E_Xor a b -> evalE q a `xor` evalE q b

eList :: Int -> [W3] -> Int
eList acc = \case
  [] -> acc
  W3 n : xs -> eList (acc*8 + n) xs

evalE3 :: Env -> E3 -> W3
evalE3 q = \case
  E3_Var v -> maybe (error (show("evalE3",v))) id $ Map.lookup v q
  E3_Lit w -> w
  E3_Mod8 e -> makeW3 (evalE q e `mod` 8)
