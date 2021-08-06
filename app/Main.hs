module Main where

import Build
import Build.Store
import Build.Task
import Build.Scheduler
import Build.Rebuilder (Rebuilder)
import Build.Task.Applicative
import qualified Data.Map as Map
import Control.Monad.State
import Data.Time.Clock.POSIX
import System.IO.Unsafe
import Debug.Trace

import qualified System.Posix.Process as P
import qualified System.Posix.Files as F

type MakeInfo k = (POSIXTime, Map.Map k POSIXTime)

-- | This rebuilder uses modification time to decide whether a key is dirty and
-- needs to be rebuilt. Used by Make.
modTimeRebuilder :: Ord k => Rebuilder Applicative (MakeInfo k) k v
modTimeRebuilder key value task = Task $ \fetch -> do
    (now, modTimes) <- get
    let dirty = case Map.lookup key modTimes of
            Nothing -> True
            time -> any (\d -> Map.lookup d modTimes > time) (dependencies task)
    if not dirty
    then return value
    else do
        put ((unsafePerformIO getPOSIXTime), Map.insert key now modTimes)
        run task fetch


make :: Ord k => Build Applicative (MakeInfo k) k v
make = topological modTimeRebuilder

inputs :: Store (MakeInfo String) String (IO ())
inputs = initialise (unsafePerformIO getPOSIXTime, Map.empty) $ \k ->
  case Map.lookup k (Map.singleton  "test.c" ()) of
    Nothing -> error "No key found"
    Just x -> pure x

tasks :: Tasks Applicative String (IO ())
tasks "test.o" = Just test_o
tasks _ = Nothing

test_o :: Task Applicative String (IO ())
test_o = Task $ (\fetch -> pure $ P.executeFile "gcc" True [ "test.c", "-o", "test.o" ] Nothing)

main :: IO ()
main = getValue "test.o" $ make tasks "test.o" inputs
       
