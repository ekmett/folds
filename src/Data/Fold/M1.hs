{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.M1
  ( M1(..)
  , runM1
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
import Data.Pointed
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep as Profunctor
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Reflection
import Data.Semigroup.Foldable
import Data.Semigroupoid
import Prelude hiding (id,(.))
import Unsafe.Coerce

-- | A semigroup reducer
data M1 a b = forall m. M1 (m -> b) (a -> m) (m -> m -> m)

instance Scan M1 where
  run1 a (M1 k h _) = k (h a)
  prefix1 a (M1 k h m) = case h a of
     x -> M1 (\y -> k (m x y)) h m
  postfix1 (M1 k h m) a = case h a of
     y -> M1 (\x -> k (m x y)) h m
  interspersing a (M1 k h m) = M1 k h m' where
    m' x y = x `m` h a `m` y
  {-# INLINE run1 #-}
  {-# INLINE prefix1 #-}
  {-# INLINE postfix1 #-}
  {-# INLINE interspersing #-}

instance Functor (M1 a) where
  fmap f (M1 k h m) = M1 (f.k) h m
  {-# INLINE fmap #-}
  b <$ _ = pure b
  {-# INLINE (<$) #-}

instance Pointed (M1 a) where
  point x = M1 (\() -> x) (\_ -> ()) (\() () -> ())
  {-# INLINE point #-}

instance Apply (M1 a) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
  (<.) m = \_ -> m
  {-# INLINE (<.) #-}
  _ .> m = m
  {-# INLINE (.>) #-}

instance Applicative (M1 a) where
  pure x = M1 (\() -> x) (\_ -> ()) (\() () -> ())
  {-# INLINE pure #-}
  M1 kf hf mf <*> M1 ka ha ma = M1
    (\(Pair' x y) -> kf x (ka y))
    (\a -> Pair' (hf a) (ha a))
    (\(Pair' x1 y1) (Pair' x2 y2) -> Pair' (mf x1 x2) (ma y1 y2))
  (<*) m = \ _ -> m
  {-# INLINE (<*) #-}
  _ *> m = m
  {-# INLINE (*>) #-}

instance Monad (M1 a) where
  return x = M1 (\() -> x) (\_ -> ()) (\() () -> ())
  {-# INLINE return #-}
  m >>= f = M1 (\xs a -> walk xs (f a)) Tip1 Bin1 <*> m
  {-# INLINE (>>=) #-}
  _ >> n = n
  {-# INLINE (>>) #-}

instance MonadZip (M1 a) where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

instance Semigroupoid M1 where
  o = (.)
  {-# INLINE o #-}

instance Category M1 where
  id = M1 id id const
  {-# INLINE id #-}
  M1 k h m . M1 k' h' m' = M1 (\(Pair' b _) -> k b) h'' m'' where
    m'' (Pair' a b) (Pair' c d) = Pair' (m a c) (m' b d)
    h'' a = Pair' (h (k' d)) d where d = h' a
  {-# INLINE (.) #-}

instance Arrow M1 where
  arr h = M1 h id const
  {-# INLINE arr #-}
  first (M1 k h m) = M1 (first k) (first h) m' where
    m' (a,_) (c,b) = (m a c, b)
  {-# INLINE first #-}
  second (M1 k h m) = M1 (second k) (second h) m' where
    m' (_,b) (a,c) = (a, m b c)
  {-# INLINE second #-}
  M1 k h m *** M1 k' h' m' = M1 (k *** k') (h *** h') m'' where
    m'' (a,b) (c,d) = (m a c, m' b d)
  {-# INLINE (***) #-}
  M1 k h m &&& M1 k' h' m' = M1 (k *** k') (h &&& h') m'' where
    m'' (a,b) (c,d) = (m a c, m' b d)
  {-# INLINE (&&&) #-}

instance Profunctor M1 where
  dimap f g (M1 k h m) = M1 (g.k) (h.f) m
  {-# INLINE dimap #-}
  lmap f (M1 k h m) = M1 k (h.f) m
  {-# INLINE lmap #-}
  rmap g (M1 k h m) = M1 (g.k) h m
  {-# INLINE rmap #-}
  ( #. ) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Strong M1 where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance Choice M1 where
  left' (M1 k h m) = M1 (_Left %~ k) (_Left %~ h) step where
    step (Left x) (Left y) = Left (m x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (M1 k h m) = M1 (_Right %~ k) (_Right %~ h) step where
    step (Right x) (Right y) = Right (m x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance ArrowChoice M1 where
  left (M1 k h m) = M1 (_Left %~ k) (_Left %~ h) step where
    step (Left x) (Left y) = Left (m x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left #-}

  right (M1 k h m) = M1 (_Right %~ k) (_Right %~ h) step where
    step (Right x) (Right y) = Right (m x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right #-}

walk :: Tree1 a -> M1 a b -> b
walk xs0 (M1 k h m) = k (go xs0) where
  go (Tip1 a) = h a
  go (Bin1 xs ys) = m (go xs) (go ys)
{-# INLINE walk #-}

runM1 :: Foldable1 f => f a -> M1 a b -> b
runM1 p (M1 k h (m :: m -> m -> m)) = reify m $ \ (_ :: Proxy s) -> k $ runS (foldMap1 (S #. h) p :: S m s)

instance Closed M1 where
  closed (M1 k h m) = M1 (\f x -> k (f x)) (fmap h) (liftA2 m)

instance Distributive (M1 a) where
  distribute fm = M1 (\t -> let g = foldDeRef1 t in runM1 g <$> fm) Tip1 Bin1
  {-# INLINE distribute #-}

instance Cosieve M1 FreeSemigroup where
  cosieve = flip runM1

instance Profunctor.Corepresentable M1 where
  type Corep M1 = FreeSemigroup
  cotabulate f = M1 (f . foldDeRef1) Tip1 Bin1

instance Functor.Representable (M1 a) where
  type Rep (M1 a) = FreeSemigroup a
  tabulate = cotabulate
  index = cosieve

instance Costrong M1 where
  unfirst = unfirstCorep
  unsecond = unsecondCorep

instance MonadReader (FreeSemigroup a) (M1 a) where
  ask = askRep
  local = localRep

instance MonadFix (M1 a) where
  mfix = mfixRep
