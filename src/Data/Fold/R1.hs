{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.R1
  ( R1(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.Zip
import Data.Distributive
import Data.Fold.Class
import Data.Fold.Internal
import Data.Functor.Apply
import Data.Functor.Rep as Functor
import Data.List.NonEmpty as NonEmpty
import Data.Pointed
import Data.Profunctor
import Data.Profunctor.Closed
import Data.Profunctor.Rep as Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Data.Semigroupoid
import Prelude hiding (id,(.))
import Unsafe.Coerce

-- | A reversed Mealy machine
data R1 a b = forall c. R1 (c -> b) (a -> c -> c) (a -> c)

instance Scan R1 where
  run1 a (R1 k _ z) = k (z a)
  prefix1 a (R1 k h z) = R1 (\c -> k (h a c)) h z
  postfix1 (R1 k h z) a = R1 k h (\c -> h c (z a))
  interspersing a (R1 k h z) = R1 k (\b x -> h b (h a x)) z
  {-# INLINE run1 #-}
  {-# INLINE prefix1 #-}
  {-# INLINE postfix1 #-}
  {-# INLINE interspersing #-}

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
    (\a ~(Pair' x y) -> Pair' (hf a x) (ha a y))
    (\a -> Pair' (zf a) (za a))
  (<*) m = \ _ -> m
  {-# INLINE (<*) #-}
  _ *> m = m
  {-# INLINE (*>) #-}

instance Monad (R1 a) where
  return x = R1 (\() -> x) (\_ () -> ()) (\_ -> ())
  {-# INLINE return #-}
  m >>= f = R1 (\xs a -> walk xs (f a)) Cons1 Last <*> m
  {-# INLINE (>>=) #-}
  _ >> n = n
  {-# INLINE (>>) #-}

instance MonadZip (R1 a) where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

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
    h' (a,_) (c,b) = (h a c, b)
  {-# INLINE first #-}
  second (R1 k h z) = R1 (second k) h' (second z) where
    h' (_,b) (a,c) = (a, h b c)
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
  lmap f (R1 k h z) = R1 k (h.f) (z.f)
  {-# INLINE lmap #-}
  rmap g (R1 k h z) = R1 (g.k) h z
  {-# INLINE rmap #-}
  ( #. ) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Strong R1 where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance Choice R1 where
  left' (R1 k h z) = R1 (_Left %~ k) step (_Left %~ z) where
    step (Left x) (Left y) = Left (h x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (R1 k h z) = R1 (_Right %~ k) step (_Right %~ z) where
    step (Right x) (Right y) = Right (h x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance ArrowChoice R1 where
  left (R1 k h z) = R1 (_Left %~ k) step (_Left %~ z) where
    step (Left x) (Left y) = Left (h x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left #-}

  right (R1 k h z) = R1 (_Right %~ k) step (_Right %~ z) where
    step (Right x) (Right y) = Right (h x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right #-}

walk :: List1 a -> R1 a b -> b
walk xs0 (R1 k h z) = k (go xs0) where
  go (Last a) = z a
  go (Cons1 a as) = h a (go as)
{-# INLINE walk #-}

instance Closed R1 where
  closed (R1 k h z) = R1 (\f x -> k (f x)) (liftA2 h) (fmap z)

instance Cosieve R1 NonEmpty where
  cosieve (R1 k h z) l = k (cata h z l)

cata :: (a -> c -> c) -> (a -> c) -> NonEmpty a -> c
cata f0 z0 (a0 :| as0) = go f0 z0 a0 as0 where
  go _ z a [] = z a
  go f z a (b:bs) = f a (go f z b bs)

instance Costrong R1 where
  unfirst = unfirstCorep
  unsecond = unsecondCorep

instance Profunctor.Corepresentable R1 where
  type Corep R1 = NonEmpty
  cotabulate f = R1 (f . NonEmpty.fromList . Prelude.reverse) (:) pure
  {-# INLINE cotabulate #-}

instance Distributive (R1 a) where
  distribute = distributeRep

instance Functor.Representable (R1 a) where
  type Rep (R1 a) = NonEmpty a
  tabulate = cotabulate
  index = cosieve

instance MonadReader (NonEmpty a) (R1 a) where
  ask = askRep
  local = localRep
