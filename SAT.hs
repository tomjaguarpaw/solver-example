module SAT where

import qualified VarMap as F
import qualified Ersatz as E
import qualified Ersatz.Solver.Toysat as Toysat
import Control.Applicative ((<*>))
import qualified Control.Monad.Trans.State as S
import qualified Data.Profunctor           as P
import qualified Data.Default              as Default
import           Data.Word                 (Word8)

data F c r = F (c -> S.State E.SAT (E.Bit, E.Solution -> Maybe r))

solve :: VarMap c r -> (c -> E.Bit) -> IO (E.Result, Maybe r)
solve (F.VarMap f) = solveF
                     . P.lmap (const id)
                     . f
                     . P.rmap (const id)
                     . formula

formula :: (c -> E.Bit) -> F c ()
formula f = F (\c -> return (f c, const (Just ())))

solveF :: F () r -> IO (E.Result, Maybe r)
solveF (F f) = do
  (res, litMap) <- E.minisat problem
--  (res, litMap) <- Toysat.toysat problem
--  (res, litMap) <- E.depqbf problem
  let solution  = E.solutionFrom litMap problem

  return (res, readSolution solution)

  where (readSolution, problem)  = flip S.runState Default.def $ do
          (bit, readSolution') <- f ()
          E.assert bit
          return readSolution'

instance P.Profunctor F where
  rmap = rmapF
  lmap = lmapF

rmapF :: (r -> r') -> F c r -> F c r'
rmapF f (F g) = F ((fmap . fmap . P.second' . fmap . fmap) f g)

lmapF :: (c' -> c) -> F c r -> F c' r
lmapF f (F g) = F (P.lmap f g)

type VarMap = F.VarMapG F

varB :: F c (Bool -> r) -> F (E.Bit -> c) r
varB (F f) = F (\c -> do
    b <- E.exists
    (eqns, vars) <- f (c b)
    return (eqns, \s -> vars s <*> E.decode s b))

varBV :: VarMap E.Bit Bool
varBV = F.VarMap varB

varBit8 :: F c (Word8 -> r) -> F (E.Bit8 -> c) r
varBit8 (F f) = F (\c -> do
    b <- E.exists
    (eqns, vars) <- f (c b)
    return (eqns, \s -> vars s <*> E.decode s b))

varBit8V :: VarMap E.Bit8 Word8
varBit8V = F.VarMap varBit8
