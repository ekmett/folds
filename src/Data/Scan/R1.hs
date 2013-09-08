{-# LANGUAGE ExistentialQuantification #-}
module Data.Scan.R1
  ( R1(..)
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

data R1 a b = forall c. R1 (c -> b) (a -> c -> c) (a -> c)

instance Functor (R1 a) where
  fmap f (R1 k h z) = R1 (f.k) h z
  {-# INLINE fmap #-}
  b <$ _ = pure b
  {-# INLINE (<$) #-}

instance Pointed (R1 a) where
  point x = R1 (\() -> x) (\_ () -> ()) (\_ -> ())
  {-# INLINE point #-}

instance Apply (R1 a) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
  (<.) m = \_ -> m
  {-# INLINE (<.) #-}
  _ .> m = m
  {-# INLINE (.>) #-}

instance Applicative (R1 a) where
  pure x = R1 (\() -> x) (\_ () -> ()) (\_ -> ())
  {-# INLINE pure #-}
  R1 kf hf zf <*> R1 ka ha za = R1
    (\(Pair' x y) -> kf x (ka y))
    (\a (Pair' x y) -> Pair' (hf a x) (ha a y))
    (\a -> Pair' (zf a) (za a))
  (<*) m = \ _ -> m
  {-# INLINE (<*) #-}
  _ *> m = m
  {-# INLINE (*>) #-}

instance Semigroupoid R1 where
  o = (.)
  {-# INLINE o #-}

instance Category R1 where
  id = arr id
  {-# INLINE id #-}
  R1 k h z . R1 k' h' z' = R1 (\(Pair' b _) -> k b) h'' z'' where
    z'' a = Pair' (z (k' b)) b where b = z' a
    h'' a (Pair' c d) = Pair' (h (k' d') c) d' where d' = h' a d
  {-# INLINE (.) #-}

instance Arrow R1 where
  arr h = R1 h const id
  {-# INLINE arr #-}
  first (R1 k h z) = R1 (first k) h' (first z) where
    h' (a,b) (c,_) = (h a c, b)
  {-# INLINE first #-}
  second (R1 k h z) = R1 (second k) h' (second z) where
    h' (a,b) (_,c) = (a, h b c)
  {-# INLINE second #-}
  R1 k h z *** R1 k' h' z' = R1 (k *** k') h'' (z *** z') where 
    h'' (a,b) (c,d) = (h a c, h' b d)
  {-# INLINE (***) #-}
  R1 k h z &&& R1 k' h' z' = R1 (k *** k') h'' (z &&& z') where
    h'' a (c,d) = (h a c, h' a d)
  {-# INLINE (&&&) #-}

instance Profunctor R1 where
  dimap f g (R1 k h z) = R1 (g.k) (h.f) (z.f)
  {-# INLINE dimap #-}
