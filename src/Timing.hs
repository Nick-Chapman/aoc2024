
module Timing where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import Text.Printf (printf)

data Timing = Timing { day :: Int, time :: Nanos }

timed :: Int -> IO () -> IO Timing
timed day io = do
  before <- getTime Monotonic
  io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let time = Nanos (gig * sec + nsec)
  pure $ Timing {day,time}

printTimings :: [Timing] -> IO ()
printTimings xs = do
  putStrLn "\ntimings:"
  forM_ (sortBy (comparing time) xs) $ \Timing{day,time} -> do
    putStrLn (printf "- day %2d : " day ++ show time)
  putStrLn $ "\ntotal = " ++ show (sum (map time xs))

newtype Nanos = Nanos Int64 deriving (Eq,Ord,Num)

instance Show Nanos where
  show (Nanos i) = printf "%.03fs" dub
    where dub :: Double = fromIntegral i / fromIntegral gig

gig :: Int64
gig = 1_000_000_000
