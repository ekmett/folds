{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Fold.L1
  ( L1(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Zip
import Data.Distributive
import Data.Fold.Class
import Data.Fold.Internal
import Data.Functor.Apply
import Data.Functor.Rep as Functor
import Data.List.NonEmpty as NonEmpty
import Data.Pointed
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Rep as Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Data.Semigroupoid
import Prelude hiding (id,(.))
import Unsafe.Coerce

-- | A Mealy Machine
data L1 a b = forall c. L1 (c -> b) (c -> a -> c) (a -> c)

instance Scan L1 where
  run1 a (L1 k _ z) = k (z a)
  prefix1 a (L1 k h z) = L1 k h (h (z a))
  postfix1 (L1 k h z) a = L1 (\c -> k (h c a)) h z
  interspersing a (L1 k h z) = L1 k (\x b -> h (h x a) b) z
  {-# INLINE run1 #-}
  {-# INLINE prefix1 #-}
  {-# INLINE postfix1 #-}
  {-# INLINE interspersing #-}

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

instance Monad (L1 a) where
  return = pure
  {-# INLINE return #-}
  m >>= f = L1 (\xs a -> walk xs (f a)) Snoc1 First <*> m
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadZip (L1 a) where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

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
    h' (a,_) (c,b) = (h a c, b)
  {-# INLINE first #-}
  second (L1 k h z) = L1 (second k) h' (second z) where
    h' (_,b) (a,c) = (a, h b c)
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
  lmap f (L1 k h z) = L1 k (\a -> h a . f) (z.f)
  {-# INLINE lmap #-}
  rmap g (L1 k h z) = L1 (g.k) h z
  {-# INLINE rmap #-}
  ( #. ) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Strong L1 where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance Choice L1 where
  left' (L1 k h z) = L1 (_Left %~ k) step (_Left %~ z) where
    step (Left x) (Left y) = Left (h x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (L1 k h z) = L1 (_Right %~ k) step (_Right %~ z) where
    step (Right x) (Right y) = Right (h x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance ArrowChoice L1 where
  left (L1 k h z) = L1 (_Left %~ k) step (_Left %~ z) where
    step (Left x) (Left y) = Left (h x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left #-}

  right (L1 k h z) = L1 (_Right %~ k) step (_Right %~ z) where
    step (Right x) (Right y) = Right (h x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right #-}

walk :: SnocList1 a -> L1 a b -> b
walk xs0 (L1 k h z) = k (go xs0) where
  go (First a) = z a
  go (Snoc1 as a) = h (go as) a
{-# INLINE walk #-}

instance Cosieve L1 NonEmpty where
  cosieve (L1 k h z) (a :| as) = k (foldl h (z a) as)

instance Costrong L1 where
  unfirst = unfirstCorep
  unsecond = unsecondCorep

instance Profunctor.Corepresentable L1 where
  type Corep L1 = NonEmpty
  cotabulate f = L1 (f . NonEmpty.fromList . Prelude.reverse) (flip (:)) pure
  {-# INLINE cotabulate #-}

instance Distributive (L1 a) where
  distribute = distributeRep

instance Functor.Representable (L1 a) where
  type Rep (L1 a) = NonEmpty a
  tabulate = cotabulate
  index = cosieve

instance Closed L1 where
  closed (L1 k h z) = L1 (\f x -> k (f x)) (liftA2 h) (fmap z)

instance MonadReader (NonEmpty a) (L1 a) where
  ask = askRep
  local = localRep

instance MonadFix (L1 a) where
  mfix = mfixRep
