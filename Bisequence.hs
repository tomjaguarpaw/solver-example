{-# LANGUAGE GADTs #-}

-- Originally from my Websockets GUI thing.  Remember to merge.

module Bisequence where

import Control.Applicative
import Data.Biapplicative
import Unsafe.Coerce
import qualified Data.Traversable as T

data Something where
  Something :: a -> Something

somethingCoerce :: Something -> a
somethingCoerce s = case s of Something a -> unsafeCoerce a

newtype Collapse f a = Collapse { unCollapse :: f a a }

instance Bifunctor p => Functor (Collapse p) where
  fmap f = Collapse . bimap f f . unCollapse

instance Biapplicative p => Applicative (Collapse p) where
  pure x = Collapse (bipure x x)
  f <*> x = Collapse (unCollapse f <<*>> unCollapse x)

biSequence :: (Biapplicative p, T.Traversable t) => t (p a b) -> p (t a) (t b)
biSequence = bimap (fmap somethingCoerce) (fmap somethingCoerce)
             . unCollapse
             . T.sequenceA
             . fmap (Collapse . bimap Something Something)

test :: ([Int], [Int])
test = biSequence [(1, 2), (3, 4), (5, 6)]
