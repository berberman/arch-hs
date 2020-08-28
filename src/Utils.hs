module Utils
  ( runHsM,
    mapSnd,
    toLower',
    isExe,
    isExeBuildTools,
    isLib,
    isLibBuildTools,
    isTest,
    isTestBuildTools,
    isBenchmark,
    isBenchmarkBuildTools,
    unExe,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (toLower)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.PackageName (PackageName)
import Types

runHsM :: r -> ExceptT e (ReaderT r m) a -> m (Either e a)
runHsM = flip (runReaderT . runExceptT)

unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

toLower' :: String -> String
toLower' = fmap toLower

isExe :: DependencyType -> Bool
isExe (Exe _) = True
isExe _ = False

isExeBuildTools :: DependencyType -> Bool
isExeBuildTools (ExeBuildTools _) = True
isExeBuildTools _ = False

isLib :: DependencyType -> Bool
isLib Lib = True
isLib _ = False

isTest :: DependencyType -> Bool
isTest (Test _) = True
isTest _ = False

isBenchmark :: DependencyType -> Bool
isBenchmark (Benchmark _) = True
isBenchmark _ = False

isLibBuildTools :: DependencyType -> Bool
isLibBuildTools LibBuildTools = True
isLibBuildTools _ = False

isTestBuildTools :: DependencyType -> Bool
isTestBuildTools (TestBuildTools _) = True
isTestBuildTools _ = False

isBenchmarkBuildTools :: DependencyType -> Bool
isBenchmarkBuildTools (BenchmarkBuildTools _) = True
isBenchmarkBuildTools _ = False