module Day24 (main) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Misc (check)
import Par4 (parse,Par,terminated,separated,nl,some,sat,lit,key,alts)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Text.Printf (printf)
import Data.List (sort,intercalate)
import System.IO (hFlush,stdout)

import Data.Set (Set,(\\))
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day24.sample"
  part1_sam <- part1 12 sam
  print ("day24, part1 (sample)", check 2024 $ part1_sam)

  inp <- parse gram <$> readFile "input/day24.input"
  part1_inp <- part1 45 inp
  print ("day24, part1", check 42410633905894 $ part1_inp)

  part2_inp <- part2 45 inp
  print ("day24, part2", check "cqm,mps,vcv,vjv,vwp,z13,z19,z25" $ part2_inp)


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
  run $ do
    let znames = [ mkZname i | i <- [ 0.. zmax ] ]
    zfs <- circuitFunction [] znames circuit
    bs <- sequence [ eval env zf | zf <- zfs ]
    pure (fromBs bs)

part2 :: Int -> Input -> IO String
part2 zmax _input@Input{circuit} = do

  let all = Map.keys circuit
  let znames = [ mkZname i | i <- [ 0.. zmax ] ]

  let
    nwrong expected pairs = do
      actual <- circuitFunction pairs znames circuit
      let is = [ i | (i,(a,e)) <- zip [0::Int .. ] (zip actual expected), a/=e ]
      pure (length is)

  let
    stage (_k::Int) lastN goalN lastPairs = do
      run $ do
        expected <- referenceAdditionCircuit zmax
        let i = 1 + zmax - lastN
        let reach = reachFrom lastPairs [ mkZname i | i <- [0..i-1] ] circuit
        let candNames = Set.toList (Set.fromList all \\ reach)
        let candidates = [ (a,b) | a <- candNames, b <- candNames, a < b ]
        let e = expected !! i
        let
          predSlow candPair = do
            --Io(putStr"(slow)")
            n <- nwrong expected (lastPairs++[candPair])
            pure (n <= goalN)
        let
          predQuick candPair = do
            let pairs = lastPairs++[candPair]
            -- ORIG: a <- (!!i) <$> circuitFunction pairs znames circuit
            -- Massive speedup (x7): only build and compare the least-significant wrong output
            a <- head <$> circuitFunction pairs [mkZname i] circuit
            pure (a == e)
        let
          pred x = do
            predQuick x >>= \case
              False -> pure False
              True -> predSlow x

        newPair <- search pred candidates
        newN <- nwrong expected (lastPairs++[newPair])
        --Print(newN)
        pure (newN,newPair)

  n0 <- run $ do
    expected <- referenceAdditionCircuit zmax
    nwrong expected []

  (n1,p1) <- stage 1 n0 (n0-1) []
  --print (check p1 ("vcv","z13"))

  (n2,p2) <- stage 2 n1 (n1-1) [p1]
  --print (check p2 ("vwp","z19"))

  (n3,p3) <- stage 3 n2 (n2-1) [p1,p2]
  --print (check p3 ("mps","z25"))

  (_n4,p4) <- stage 4 n3 0 [p1,p2,p3]
  --print (check p4 ("cqm","vjv"))
  --print (check _n4 0)

  pure $ intercalate "," $ sort [ n | (n1,n2) <- [p1,p2,p3,p4], n <- [n1,n2] ]


search :: (a -> M Bool) -> [a] -> M a
search pred cands =
  do
    Io (printf "#%d:" (length cands))
    loop (zip [0::Int ..] cands) -- reverse cands 22s-->12s !
  where
    flush = hFlush stdout
    dot = Io $ do putStr "."; flush
    loop = \case
      [] -> error "search!"
      (i,x):more -> do
        pred x >>= \case
          True -> do
            Io(putStrLn(show i))
            pure x
          False -> do
            if i `mod` 1000 /= 0 then pure () else do Io(putStr(show i)); dot
            loop more


referenceAdditionCircuit :: Int -> M [F]
referenceAdditionCircuit zmax = do
  let xs = [ x n | n <- [0..zmax-1] ]
  let ys = [ y n | n <- [0..zmax-1] ]
  a <- mapM var xs
  b <- mapM var ys
  add a b

add :: [F] -> [F] -> M [F]
add xs ys = loop zero (zip xs ys) where
  loop :: F -> [(F,F)] -> M [F]
  loop cin = \case
    [] -> pure [cin] -- dont drop final carry out
    (x,y):rest -> do
      cout <- majority cin x y
      z <- parity cin x y
      zs <- loop cout rest
      pure (z:zs)


assignmentEnv :: Map Name Bool -> Env
assignmentEnv ass = Map.fromList [ (varOfNameXY n, b) | (n,b) <- Map.toList ass ]

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

fromBs :: [Bool] -> Int
fromBs = \case [] -> 0; b:bs -> (if b then 1 else 0) + 2 * fromBs bs

look :: String -> Name -> Map Name v -> v
look tag n m = maybe err id $ Map.lookup n m
  where err = error (show ("look",tag,n))

maybeVarName :: String -> Maybe Var
maybeVarName = \case
  ['x',c2,c3] -> Just (x (read [c2,c3]))
  ['y',c2,c3] -> Just (y (read [c2,c3]))
  _ -> Nothing

varOfNameXY :: String -> Var
varOfNameXY s = case maybeVarName s of Just v -> v; Nothing -> error ("varOfNameXY:"++s)

mkZname :: Int -> Name
mkZname n = printf "z%02d" n

type MF = Map Name F

type Rename = Name -> Name


reachFrom :: [(Name,Name)] -> [Name] -> Map Name Driver -> Set Name
reachFrom pairs outputs circuit = collect Set.empty outputs
  where
    rename :: Rename
    rename = do
      let f acc (n0,n1) k = if k==n0 then n1 else if k==n1 then n0 else acc k
      foldl f id pairs

    collect :: Set Name -> [Name] -> Set Name
    collect acc = \case
      [] -> acc
      x:xs -> collect (collect1 acc x) xs

    collect1 :: Set Name -> Name -> Set Name
    collect1 acc0 name0 = do
      let name = rename name0
      if name `Set.member` acc0 then acc0 else do
        let acc = Set.insert name acc0
        case maybeVarName name of
          Just{} -> acc
          Nothing ->
            case look "reachFrom" name circuit of
              And x y -> collect1 (collect1 acc x) y
              Or x y -> collect1 (collect1 acc x) y
              Xor x y -> collect1 (collect1 acc x) y


circuitFunction :: [(Name,Name)] -> [Name] -> Map Name Driver -> M [F]
circuitFunction pairs outputs circuit = builds Map.empty outputs
  where
    rename :: Rename
    rename = do
      let f acc (n0,n1) k = if k==n0 then n1 else if k==n1 then n0 else acc k
      foldl f id pairs

    builds :: MF -> [Name] -> M [F]
    builds mf = \case
      [] -> pure []
      x:xs -> do
        (mf,y) <- build mf x
        ys <- builds mf xs
        pure (y:ys)

    build :: MF -> Name -> M (MF,F)
    build mf name0 = do
      let name = rename name0
      case Map.lookup name mf of
        Just f -> do
          pure (mf,f)
        Nothing -> do
          case maybeVarName name of
            Just v -> do
              f <- var v
              pure (mf,f)
            Nothing -> do
              (mf,f) <- do
                mf <- pure $ Map.insert name 0 mf -- break cycles which might be introduced by swapping
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
              let mf' = Map.insert name f mf
              pure (mf',f)

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
  x' <- inv x
  y' <- inv y
  a <- conj x y'
  b <- conj y x'
  disj a b

disj :: F -> F -> M F
disj x y = do
  x' <- inv x
  y' <- inv y
  conj x' y' >>= inv

type Memo = Map F F

inv :: F -> M F
inv a = do (_,res) <- loop Map.empty a; pure res
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

type Memo2 = Map (F,F) F

conj :: F -> F -> M F
conj p q = do (_,res) <- loop Map.empty p q; pure res
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

data Prefix = X | Y deriving (Eq,Ord,Show)
type Var = (Int,Prefix) -- so we intereave X/Y for sorting!

x,y :: Int -> Var
x n = (n,X)
y n = (n,Y)

ite :: Var -> F -> F -> M F
ite i t e = if t == e then pure t else Construct (i,t,e)

var :: Var -> M F
var v = Construct (v,one,zero)

zero,one :: F
zero = 0
one = 1

data Form a = Zero | One | Ite Var a a deriving Show

instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Construct :: (Var,F,F) -> M F
  Destruct :: F -> M (Form F)
  Print :: Show a => a -> M ()
  Io :: IO a -> M a

run :: M a -> IO a
run m0 = loop m0 state0 (\_ -> pure)
  where
    loop :: M a -> State -> (State -> a -> IO b) -> IO b
    loop m0 s k = case m0 of
      Ret a -> k s a
      Bind m g -> loop m s $ \s a -> loop (g a) s k
      Construct trip@(i,t,e) -> do
        let State{up} = s
        case (Map.lookup (i,t,e) up) of
          Just f -> do
            let State{hit} = s
            k s { hit = 1 + hit } f
          Nothing -> do
            let State{u,up,down} = s
            k s { u = u + 1, up = Map.insert trip u up, down = Map.insert u trip down } u
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
        --let State{hit,u} = s
        --printf "%d: #%d: %s\n" hit u (show x)
        print x
        k s ()
      Io io -> do a <- io; k s a

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
