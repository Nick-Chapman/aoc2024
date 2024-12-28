module Bdd (main) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Misc (check)
import Par4 (parse,Par,terminated,separated,nl,some,sat,lit,key,alts)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Text.Printf (printf)

--look :: (Eq k, Ord k , Show k) => String -> k -> Map k v -> v
look :: String -> Name -> Map Name v -> v
look tag n m = maybe err id $ Map.lookup n m
  where err = error (show ("look",tag,n))

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day24.sample"
  part1_sam <- part1 12 sam
  print ("day24, part1 (sample)", check 2024 $ part1_sam)

  inp <- parse gram <$> readFile "input/day24.input"
  part1_inp <- part1 45 inp
  print ("day24, part1", check 42410633905894 $ part1_inp)


type Name = String
data Driver = And Name Name | Or Name Name | Xor Name Name deriving Show
data Input = Input
  { assignment :: Map Name Bool
  , circuit :: Map Name Driver
  } deriving Show

gram :: Par Input
gram = do
  assignment <- Map.fromList <$> terminated nl inp; nl
  circuit <- Map.fromList <$> separated nl gate
  pure Input { assignment, circuit }
  where
    name = some $ sat (\c -> Char.isAlpha c || Char.isDigit c)
    inp :: Par (Name,Bool)
    inp = do
      n <- name
      key ": "
      b <- alts [ do lit '0'; pure False; , do lit '1'; pure True ]
      pure (n,b)
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


part1 :: Int -> Input -> IO Int
part1 zmax Input{assignment,circuit} = do
  let env = assignmentEnv assignment
  --let (width,_) :: Var = maximum [ v | (v,_) <- env ]
  --let zmax = 12
  n <- run $ do
    mf <- circuitFunction zmax circuit
    --Print mf
    --Dump "after circuit"
    let zfs = [ look "part1" (mkZname i) mf | i <- [0..zmax] ]
    bs <- sequence [ eval env zf | zf <- zfs ]
    --Print bs
    pure $ fromBs bs

  --print n
  pure n


maybeVarName :: String -> Maybe Var
maybeVarName = \case
  ['x',c2,c3] -> Just (x (read [c2,c3]))
  ['y',c2,c3] -> Just (y (read [c2,c3]))
  _ -> Nothing

varOfNameXY :: String -> Var
varOfNameXY s = case maybeVarName s of Just v -> v; Nothing -> error ("varOfNameXY:"++s)

mkZname :: Int -> Name
mkZname n = printf "z%02d" n

assignmentEnv :: Map Name Bool -> Env
assignmentEnv ass = Map.fromList [ (varOfNameXY n, b) | (n,b) <- Map.toList ass ]

type MF = Map Name F

circuitFunction :: Int -> Map Name Driver -> M (Map Name F)
circuitFunction zmax circuit = builds Map.empty [ mkZname i | i <- [ 0.. zmax ] ]
  where
    builds :: MF -> [Name] -> M MF
    builds mf = \case
      [] -> pure mf
      x:xs -> do
        --Print x
        (mf,_) <- build mf x
        builds mf xs

    build :: MF -> Name -> M (MF,F)
    build mf name = do
      --Print ("build",name)
      case Map.lookup name mf of
        Just f -> do
          --Print ("build",name, "HIT")
          pure (mf,f)
        Nothing -> do
          case maybeVarName name of
            Just v -> do
              --Print ("build",name, "VAR")
              f <- var v
              pure (mf,f)
            Nothing -> do
              --Print ("build",name, "NODE")
              (mf,f) <- do
                (case look "circuitFunction" name circuit of
                  And x y -> do
                    (mf,x) <- build mf x
                    (mf,y) <- build mf y
                    f <- conj x y
                    pure (mf,f)
                  Or x y -> do
                    (mf,x) <- build mf x
                    (mf,y) <- build mf y
                    f <- disj x y
                    pure (mf,f)
                  Xor x y -> do
                    (mf,x) <- build mf x
                    (mf,y) <- build mf y
                    f <- xor x y
                    pure (mf,f))
              --Print ("build",name, "NODE/done")
              let mf' = Map.insert name f mf
              pure (mf',f)


-- TODO: do full eval/subs in one step
{-
eval q f = subs f q >>= getConst

subs :: F -> Env -> M F
subs f = \case
  [] -> pure f
  (v,b):q -> do
    Print ("sub",v)
    f' <- sub v b f
    subs f' q

-- TODO: and memoization here?
sub :: Var -> Bool -> F -> M F
sub v b f =
  Destruct f >>= \case
    Zero -> pure zero
    One -> pure one
    Ite i t e ->
      if i == v then pure (if b then t else e) else do
        t' <- sub v b t
        e' <- sub v b e
        ite i t' e'

getConst :: F -> M Bool
getConst f =
  Destruct f >>= \case
    Zero -> pure False
    One -> pure True
    Ite{} -> error "getConst"
-}

type Env = Map Var Bool

eval :: Env  -> F -> M Bool
eval env f = loop f
  where
    loop f = do
      Destruct f >>= \case
        Zero -> pure False
        One -> pure True
        Ite i t e -> do
          let b = maybe err id $ Map.lookup i env where err = error (show ("eval",i))
          loop (if b then t else e)



--toBs :: Int -> [Bool]
--toBs n = if n == 0 then [] else (if n `mod` 2 == 0 then False else True) : toBs (n `div` 2)

fromBs :: [Bool] -> Int
fromBs = \case [] -> 0; b:bs -> (if b then 1 else 0) + 2 * fromBs bs

_add :: [F] -> [F] -> M [F]
_add xs ys = loop zero (zip xs ys) where
  loop :: F -> [(F,F)] -> M [F]
  loop cin = \case
    [] -> pure [] -- drop final carry out
    (x,y):rest -> do
      cout <- majority cin x y
      z <- parity cin x y
      zs <- loop cout rest
      pure (z:zs)

majority :: F -> F -> F -> M F
majority a b c = do
  ab <- conj a b
  ac <- conj a c
  bc <- conj b c
  x <- disj ab ac
  disj x bc

parity :: F -> F -> F -> M F
parity a b c = do
  ab <- xor a b
  xor ab c

xor :: F -> F -> M F
xor x y = do
  --Print "xor"
  x' <- inv x
  y' <- inv y
  a <- conj x y'
  b <- conj y x'
  disj a b

disj :: F -> F -> M F
disj x y = do
  --Print "disj"
  x' <- inv x
  y' <- inv y
  conj x' y' >>= inv

{-
-- TODO: we need memoization as we walk over the two BDD which are being conjoined
-- orig version...
_conj :: F -> F -> M F
_conj f1 f2 = do
  --Print ("conj",f1,f2)
  f1' <- Destruct f1
  f2' <- Destruct f2
  case (f1',f2') of
    (Zero,_) -> pure zero
    (_,Zero) -> pure zero
    (One,_) -> pure f2
    (_,One) -> pure f1
    (Ite v1 t1 e1, Ite v2 t2 e2) ->
      if v1 == v2 then do
        t <- conj t1 t2
        e <- conj e1 e2
        ite v1 t e
      else if v1 < v2 then do
        t <- conj t1 f2
        e <- conj e1 f2
        ite v1 t e
      else do
        t <- conj t2 f1
        e <- conj e2 f1
        ite v2 t e
-}

type Memo2 = Map (F,F) F

conj :: F -> F -> M F
conj p q = do
  (_,res) <- loop Map.empty p q
  pure res

  where

    loop :: Memo2 -> F -> F -> M (Memo2, F)
    loop memo f1 f2 =
      case Map.lookup (f1,f2) memo of
        Just res -> pure (memo,res)
        Nothing -> do
          f1' <- Destruct f1
          f2' <- Destruct f2
          (memo,res) <- compute memo f1 f2 (f1',f2')
          let memo' = Map.insert (f1,f2) res memo
          pure (memo', res)

    compute :: Memo2 -> F -> F -> (Form F,Form F) -> M (Memo2, F)
    compute memo f1 f2 = \case
      (Zero,_) -> pure (memo,zero)
      (_,Zero) -> pure (memo,zero)
      (One,_) -> pure (memo,f2)
      (_,One) -> pure (memo,f1)
      (Ite v1 t1 e1, Ite v2 t2 e2) ->
        if v1 == v2 then do
          (memo,t) <- loop memo t1 t2
          (memo,e) <- loop memo e1 e2
          res <- ite v1 t e
          pure (memo,res)
        else if v1 < v2 then do
          (memo,t) <- loop memo t1 f2
          (memo,e) <- loop memo e1 f2
          res <- ite v1 t e
          pure (memo,res)
        else do
          (memo,t) <- loop memo t2 f1
          (memo,e) <- loop memo e2 f1
          res <- ite v2 t e
          pure (memo,res)


type Memo = Map F F
-- TODO: memoization here
inv :: F -> M F
inv a = do
  (_,res) <- loop Map.empty a
  pure res
  where
    loop :: Memo -> F -> M (Memo, F)
    loop memo f = do
      case Map.lookup f memo of
        Just res -> pure (memo,res)
        Nothing -> do
          (memo,res) <- do
            Destruct f >>= \case
              Zero -> pure (memo,one)
              One -> pure (memo,zero)
              Ite i t e -> do
                (memo, t') <- loop memo t
                (memo, e') <- loop memo e
                res <- ite i t' e'
                pure (memo, res)
          let memo' = Map.insert f res memo
          pure (memo',res)

data Prefix = X | Y deriving (Eq,Ord,Show)
type Var = (Int,Prefix) -- so we intereave X/Y for sorting!

x,y :: Int -> Var
x n = (n,X)
y n = (n,Y)

ite :: Var -> F -> F -> M F
ite i t e = do
  --Print ("ite")
  ConsIte i t e

var :: Var -> M F
var v = do
  --Print ("var",v)
  ConsIte v one zero

data Form a = Zero | One | Ite Var a a deriving Show

instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  ConsIte :: Var -> F -> F -> M F
  Destruct :: F -> M (Form F)
  Print :: Show a => a -> M ()
  Dump :: String -> M ()

zero,one :: F
zero = 0
one = 1

run :: M a -> IO a
run m0 = loop m0 state0 (\_ -> pure)
  where
    loop :: M a -> State -> (State -> a -> IO b) -> IO b
    loop m0 s k = case m0 of
      Ret a -> k s a
      Bind m g -> loop m s $ \s a -> loop (g a) s k
      ConsIte i t e -> if t == e then k s t else do
        let trip = (i,t,e)
        --print ("ConsIte",trip)
        let State{up} = s
        case (Map.lookup (i,t,e) up) of
          Just f -> do
            --print ("ConsIte",trip,"--(CACHE)-->",f)
            let State{hit} = s
            k s { hit = 1 + hit } f
          Nothing -> do
            let State{u=f,up,down} = s
            --print ("ConsIte",trip,"--(NEW)-->",f)
            let s' = s { u = f + 1
                       , up = Map.insert trip f up
                       , down = Map.insert f trip down }
            k s' f

      Destruct form ->
        case form of
          0 -> k s Zero
          1 -> k s One
          n -> do
            let State{down} = s
            let err = error (show ("Destruct",n))
            let (i,t,e) = maybe err id $ Map.lookup n down
            k s (Ite i t e)

      Print x -> do
        let State{hit,u} = s
        printf "%d: #%d: %s\n" hit u (show x)
        k s ()

      Dump tag -> do
        let State{u,up=_up,down} = s
        print (tag,"u=",u)
        --mapM_ print (Map.toList _up)
        mapM_ print (Map.toList down)
        k s ()

type F = Int
type Trip = (Var,F,F)

state0 :: State
state0 = State { u = 2, up = Map.empty, down = Map.empty, hit = 0 }

data State = State
  { u :: Int
  , up :: Map Trip F
  , down :: Map F Trip
  , hit :: Int
  } deriving Show
