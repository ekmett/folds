{-# LANGUAGE ExistentialQuantification #-}
module Data.Scan.L1
  ( L1(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Fold.Internal
import Data.Functor.Apply
import Data.Pointed
import Data.Profunctor
import Data.Semigroupoid
import Prelude hiding (id,(.))

-- | A Mealy Machine
data L1 a b = forall c. L1 (c -> b) (c -> a -> c) (a -> c)

instance Functor (L1 a) where
  fmap f (L1 k h z) = L1 (f.k) h z
  {-# INLINE fmap #-}
  b <$ _ = pure b
  {-# INLINE (<$) #-}

instance Pointed (L1 a) where
  point x = L1 (\() -> x) (\() _ -> ()) (\_ -> ())
  {-# INLINE point #-}

instance Apply (L1 a) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
  (<.) m = \_ -> m
  {-# INLINE (<.) #-}
  _ .> m = m
  {-# INLINE (.>) #-}

instance Applicative (L1 a) where
  pure x = L1 (\() -> x) (\() _ -> ()) (\_ -> ())
  {-# INLINE pure #-}
  L1 kf hf zf <*> L1 ka ha za = L1
    (\(Pair' x y) -> kf x (ka y))
    (\(Pair' x y) a -> Pair' (hf x a) (ha y a))
    (\a -> Pair' (zf a) (za a))
  (<*) m = \ _ -> m
  {-# INLINE (<*) #-}
  _ *> m = m
  {-# INLINE (*>) #-}

instance Semigroupoid L1 where
  o = (.)
  {-# INLINE o #-}

instance Category L1 where
  id = arr id
  {-# INLINE id #-}
  L1 k h z . L1 k' h' z' = L1 (\(Pair' b _) -> k b) h'' z'' where
    z'' a = Pair' (z (k' b)) b where b = z' a
    h'' (Pair' c d) a = Pair' (h c (k' d')) d' where d' = h' d a
  {-# INLINE (.) #-}

instance Arrow L1 where
  arr h = L1 h (\_ a -> a) id
  {-# INLINE arr #-}
  first (L1 k h z) = L1 (first k) h' (first z) where
    h' (a,b) (c,_) = (h a c, b)
  {-# INLINE first #-}
  second (L1 k h z) = L1 (second k) h' (second z) where
    h' (a,b) (_,c) = (a, h b c)
  {-# INLINE second #-}
  L1 k h z *** L1 k' h' z' = L1 (k *** k') h'' (z *** z') where
    h'' (a,b) (c,d) = (h a c, h' b d)
  {-# INLINE (***) #-}
  L1 k h z &&& L1 k' h' z' = L1 (k *** k') h'' (z &&& z') where
    h'' (c,d) a = (h c a, h' d a)
  {-# INLINE (&&&) #-}

instance Profunctor L1 where
  dimap f g (L1 k h z) = L1 (g.k) (\a -> h a . f) (z.f)
  {-# INLINE dimap #-}
