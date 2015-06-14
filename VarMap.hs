{-# LANGUAGE Rank2Types #-}

module VarMap where

import qualified Data.Profunctor           as P
import qualified Data.Biapplicative        as B

data VarMapG f a b = VarMap (forall c r. f c (b -> r) -> f (a -> c) r)

instance P.Profunctor f => B.Bifunctor (VarMapG f) where
  bimap f g (VarMap h) = VarMap ((((P.rmap . P.lmap . P.lmap) f)
                                  . ((P.lmap . P.rmap . P.lmap) g)) h)

-- Good lord
instance P.Profunctor f => B.Biapplicative (VarMapG f) where
  bipure a b = B.bimap (const a) (const b)
               (VarMap (P.lmap (P.rmap ($ ())) (P.rmap (P.lmap ($ ())) id)))
  VarMap f <<*>> VarMap g = B.bimap (uncurry ($)) (uncurry (flip ($)))
                            (VarMap ((P.lmap . P.rmap) curry
                                     (((P.rmap . P.lmap) curry) (f . g))))
