module Main where

import Build
import Build.Rebuilder (Rebuilder)
import Build.Scheduler
import Build.Store
import Build.Task
import Build.Task.Applicative
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock.POSIX
import Debug.Trace
import System.IO.Unsafe
import qualified System.Posix.Files as F
import qualified System.Posix.Process as P
import Utilities

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
      put (now + 1, Map.insert key now modTimes)
      run task fetch

make :: Ord k => Build Applicative (MakeInfo k) k v
make = topological modTimeRebuilder

deps k = maybe [] dependencies (tasks k)

list k = fromJust $ topSort $ graph deps k

-- END of copies of the build files
init :: String -> IO (Store (MakeInfo String) String (IO ()))
init key = do
  now <- getPOSIXTime
  m <- tmap
  i <- imap
  pure $
    initialise (now, i) $ \k ->
      case Map.lookup k m of
        Nothing -> error ("No key " ++ k ++ " found")
        Just x -> x
  where
    tmap :: IO (Map.Map String (IO ()))
    tmap = do
      f <- file_exists
      let l = zip f (repeat (pure ()))
      pure $ Map.fromList l
    imap :: IO (Map.Map String POSIXTime)
    imap = do
      f <- file_exists
      tmp3 <- mapM (\k -> do tmp2 <- tmp k; pure (k, tmp2)) f
      pure $ Map.fromList tmp3
    file_exists :: IO [FilePath]
    file_exists = filterM (\k -> F.fileExist k) (list key)
    tmp :: FilePath -> IO POSIXTime
    tmp k = do
      s <- F.getFileStatus k
      pure $ F.modificationTimeHiRes s

-- END of store initialization

tasks :: Tasks Applicative String (IO ())
tasks "main.o" = Just test_o
tasks "main" = Just main_elf
tasks _ = Nothing

test_o :: Task Applicative String (IO ())
test_o =
  Task $
    ( \fetch ->
        let t1 = fetch "main.c"
         in let t2 = P.executeFile "gcc" True ["main.c", "-c", "-o", "main.o"] Nothing
             in (>>) <$> t1 <*> (pure t2)
    )

main_elf :: Task Applicative String (IO ())
main_elf =
  Task $
    ( \fetch ->
        let t1 = fetch "main.o"
         in let t2 = P.executeFile "gcc" True ["main.o", "-o", "main"] Nothing
             in (>>) <$> t1 <*> (pure t2)
    )

-- END of task descriptions

main :: IO ()
main = do
  init_store <- Main.init "main"
  let end_store = make tasks "main" init_store
  getValue "main" end_store
