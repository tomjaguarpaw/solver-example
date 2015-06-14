{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Distribution.Package as Package
import qualified Distribution.Version as Version
import           Control.Monad             (when)
import qualified Solve
import           Solve (M, P, V)
import qualified Cabal

saveBytestringDeps :: IO ()
saveBytestringDeps = Cabal.saveDeps "bytestringDeps" (Package.PackageName "bytestring", Version.Version [0,10,6,0] [])

loadBytestringDeps :: IO (M P (M V (M P [V])))
loadBytestringDeps = Cabal.loadDeps "bytestringDeps"

saveOpaleyeDeps :: IO ()
saveOpaleyeDeps = Cabal.saveDeps "opaleyeDeps" (Package.PackageName "opaleye", Version.Version [0,3,1,2] [])

loadOpaleyeDeps :: IO (M P (M V (M P [V])))
loadOpaleyeDeps = Cabal.loadDeps "opaleyeDeps"

main :: IO ()
main = do
--  baz <- loadBytestringDeps
--  saveOpaleyeDeps
  baz <- loadOpaleyeDeps
  {-
  _ <- flip Map.traverseWithKey baz $ \p mvmpvs -> do
         print p
         flip Map.traverseWithKey mvmpvs $ \v mpvs -> do
           putStr "\t"
           print v
           flip Map.traverseWithKey mpvs $ \p' vs -> do
             putStr "\t\t"
             print p'
             flip T.traverse vs $ \v' -> do
               putStr "\t\t\t"
               print v'
  -}
  solution <- Solve.deduceInstallPlan baz

  case solution of
    Just solution' -> do
      _ <- flip Map.traverseWithKey solution' $ \p mvs -> do
             print p
             flip Map.traverseWithKey mvs $ \v s -> do
               when s $ do
                 putStr "\t"
                 putStrLn ("Installing " ++ show v)
      return ()
    Nothing        -> putStrLn "No solution"

  return ()

      
--  print (F.sum (Map.map (F.sum . Map.map length) m))
