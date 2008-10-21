{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
-- microbench, a tiny microbenchmarking library for Haskell.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- |Microbenchmarking can be used to compare the speed of different
-- approaches to the same operation.  Since most code is very fast, to
-- get accurate timing information you must run the operation many times
-- and then divide to get the time per operation.
--
-- This library manages the microbenchmarking process: it finds how many
-- iterations of a function are needed to get a good timing estimate per
-- iteration and prints out a human-readable \"Your code takes /n/
-- nanoseconds to run, and can run /n/ times per second\".
--
-- The only function 'microbench' takes a function that expects an
-- integer parameter (which is the quantity you're trying to measure),
-- and probes the function with increasing parameters until enough time
-- has elapsed to get a good measurement.
--
-- This may be better understood by some example code:
--
-- > sum1 n = sum [1..n]
-- > sum2 n = foldl (+) 0 [1..n]
-- > main = do
-- >   microbench "Sum using sum" sum1
-- >   microbench "Sum using foldl" sum2
--
-- When run, @sum1@ and @sum2@ are called with varying values of @n@.
-- The output, then, is an estimate of how many integers these
-- approaches could sum per second.
--
-- 'microbench' also accepts a parameter of type @IO ()@ for
-- benchmarking.  It does the same probing process, but manages running
-- the operation in a loop.
module Microbench (
  microbench,
  Microbenchable
) where

import Control.Exception
import Debug.Trace
import Data.IORef
import Data.List
import Data.Time.Clock
import Data.Typeable
import Control.Concurrent
import System.IO
import Numeric

-- Want to handle:
--   Int -> a    => ok
--   Int -> IO a => ok
--   IO ()       => loop the action
-- TODO:
--   IO a  => loop the action, forcing each result?
--   a     => is it possible? I tried for a while but couldn't make it work.
--
-- TODO 2:
--   factor out "setup time" by comparing different outputs for different inputs
--   and a linear model.

-- |Microbenchmarkable computations.  Be very wary of adding your own
-- instances of this class, as it's difficult to force GHC to
-- re-evaluate code in a way that makes benchmarking easy.
class Microbenchable a where
  run :: a -> Int -> IO ()

instance Microbenchable (Int -> IO ()) where
  run f n = f n
instance Microbenchable (Int -> a) where
  run f n = do x <- evaluate (f n); return ()
instance Microbenchable (IO ()) where
  run f n = mapM_ (const f) [1..n]

-- This was chosen totally arbitrarily.  Perhaps it would be better to make it
-- a parameter, or use some sort of real statistical test.
microbenchTime = 1

-- |@microbench description target@ probes target with different parameters
-- until it's ran enough iterations to have a good estimate at the rate per
-- second of the operation.  @description@ is a textual description of the
-- thing being benchmarked.  Outputs to stdout.
microbench :: Microbenchable a => String -> a -> IO ()
microbench desc f = do
  hSetBuffering stdout NoBuffering
  putStr $ "* " ++ desc ++ ": "
  start <- getCurrentTime
  time <- probe 1
  putStrLn ""
  putStrLn $ "  " ++ showFFloat (Just 3) (time*1000*1000) "ns per iteration / "
                  ++ showGFloat (Just 2) (1 / time) " per second."
  where
  probe repeats = do
    putStr "."
    start <- getCurrentTime
    run f repeats
    end <- getCurrentTime
    let delta = end `diffUTCTime` start
    if delta > microbenchTime
      then do return (realToFrac delta / realToFrac repeats)
      else probe (repeats * 2)

