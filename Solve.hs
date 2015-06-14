module Solve where

import qualified Ersatz as E
import qualified Distribution.Version as Version
import qualified Data.Map.Strict as Map
import qualified Data.Bifunctor            as B
import qualified Bisequence                as B
import qualified Control.Arrow as Arrow
import qualified Distribution.Package as Package
import qualified SAT as SAT
import qualified Data.Foldable as F
import qualified Data.Maybe as Maybe

type M = Map.Map
type P = Package.PackageName
type V = Version.Version
type D = Package.Dependency

type I a = a -> a

-- For each package version
-- 1) a Bit and Bool that says whether it is to be installed
-- 2) its dependencies
makeVars :: M P (M V (M P [V]))
         -> SAT.VarMap (M P (M V (E.Bit, M P [V])))
                       (M P (M V Bool))
makeVars =
  B.biSequence
  . Map.map B.biSequence
  . (id :: I (M P (M V (SAT.VarMap (E.Bit, M P [V]) Bool))))
  . Map.map (Map.map (\(vm, mpv) -> B.bimap (flip (,) mpv) id vm))
  . (id :: I (M P (M V (SAT.VarMap E.Bit Bool, M P [V]))))
  . Map.map (Map.map ((,) SAT.varBV))
  
-- For each package version, for each dependent package, whether that
-- dependent package's dependencies are satisfied.
oneVersion :: M P (M V (E.Bit, M P [V]))
           -> M P (M V (E.Bit, M P E.Bit))
oneVersion m = flip Map.map m (\mvbvmpvs ->
                 flip Map.map mvbvmpvs (\bvmpvs ->
                   flip Arrow.second bvmpvs (\mpvs ->
                     flip Map.mapWithKey mpvs (\p vs ->
                       if (case p of Package.PackageName s -> s) == "rts" then
                         E.true
                       else
                         E.or (Maybe.catMaybes (flip map vs (\v -> 
                          fmap fst (Map.lookup v =<< Map.lookup p m)))
               )))))

-- For each package version, if it is installed require that all its
-- dependencies are satisfied.
allDependencies :: M P (M V (E.Bit, M P E.Bit))
                -> M P (M V E.Bit)
allDependencies = ((Map.map . Map.map) (uncurry (E.==>))
                   . (Map.map . Map.map . Arrow.second) E.and)

-- Ensure that all installed packages have all their dependencies satisfied.
allPackages :: M P (M V (E.Bit, M P [V]))
            -> E.Bit
allPackages = E.and
              . Map.map E.and
              . allDependencies
              . oneVersion

-- Count the number of installed packages
count :: M P (M V (E.Bit, ignored)) -> E.Bits
count = F.sum
        . Map.map F.sum
        . Map.map (Map.map (E.bits . fst))

equations :: Integer -> M P (M V (E.Bit, M P [V])) -> E.Bit
equations maxInstalls m = installPackage
                          E.&& allPackages m
                          E.&& notTooMany
  where installPackage :: E.Bit
        installPackage = fst (Maybe.fromJust (Map.lookup version =<< Map.lookup package m))

--        bytestring = Package.PackageName "bytestring"
--        version = Version.Version [0,10,6,0] []

        package = Package.PackageName "opaleye"
        version = Version.Version [0,3,1,2] []

        notTooMany :: E.Bit
        notTooMany = count m E.<=? fromInteger maxInstalls


installPlan :: SAT.VarMap c r
            -> (Integer -> c -> E.Bit)
            -> IO (Maybe r)
installPlan = installPlan' Nothing 62

installPlan' :: Maybe r
             -> Integer
             -> SAT.VarMap c r
             -> (Integer -> c -> E.Bit)
             -> IO (Maybe r)
installPlan' previous maxInstalls vars eqns = do
  (result, solution) <- SAT.solve vars (eqns maxInstalls)

  case (result, solution) of
    (E.Unsolved,    _)     -> return previous
    (E.Unsatisfied, _)     -> return previous
    (E.Satisfied, Nothing) -> error "Nothing when Satisfied"
    (E.Satisfied, solution'@(Just _)) ->
      if maxInstalls == 42 then
        return solution'
      else
        installPlan' solution' (maxInstalls - 1) vars eqns

deduceInstallPlan :: M P (M V (M P [V])) -> IO (Maybe (M P (M V Bool)))
deduceInstallPlan = flip installPlan equations . makeVars
