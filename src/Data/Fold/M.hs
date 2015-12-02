{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Unlike 'Data.Fold.L' and 'Data.Fold.R' this 'Comonad'
-- is based on a @(->) r@ 'Comonad' for a 'Monoid' @r@ rather than
-- than on the @'Store' r@ 'Comonad'.
module Data.Fold.M
  ( M(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.Zip
import Data.Distributive
import Data.Fold.Class
import Data.Fold.Internal
import Data.Foldable hiding (sum, product)
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Functor.Rep as Functor
import Data.Profunctor
import Data.Profunctor.Closed
import Data.Profunctor.Rep as Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Reflection
import Unsafe.Coerce
import Prelude hiding (sum, product, length)

-- | A 'foldMap' caught in amber. a.k.a. a monoidal reducer
data M a b = forall m. M (m -> b) (a -> m) (m -> m -> m) m

instance Scan M where
  run1 a (M k h _ _) = k (h a)
  prefix1 a (M k h m z) = case h a of
     x -> M (\y -> k (m x y)) h m z
  postfix1 (M k h m z) a = case h a of
     y -> M (\x -> k (m x y)) h m z
  interspersing a (M k h m z) = M (maybe' (k z) k) h' m' Nothing' where
    h' r  = Just' (h r)
    m' (Just' x) (Just' y) = Just' (x `m` h a `m` y)
    m' Nothing' my = my
    m' mx Nothing' = mx
  {-# INLINE run1 #-}
  {-# INLINE prefix1 #-}
  {-# INLINE postfix1 #-}
  {-# INLINE interspersing #-}

-- | efficient 'prefix', efficient 'postfix'
instance Folding M where
  run s (M k h m (z :: m)) = reify (m, z) $
    \ (_ :: Proxy s) -> k $ runN (foldMap (N #. h) s :: N m s)
  runOf l s (M k h m (z :: m)) = reify (m, z) $
    \ (_ :: Proxy s) -> k $ runN (foldMapOf l (N #. h) s :: N m s)
  prefix s (M k h m (z :: m)) = reify (m, z) $
    \ (_ :: Proxy s) -> case runN (foldMap (N #. h) s :: N m s) of
      x -> M (\y -> k (m x y)) h m z
  prefixOf l s (M k h m (z :: m)) = reify (m, z) $
    \ (_ :: Proxy s) -> case runN (foldMapOf l (N #. h) s :: N m s) of
      x -> M (\y -> k (m x y)) h m z
  postfix (M k h m (z :: m)) s = reify (m, z) $
    \ (_ :: Proxy s) -> case runN (foldMap (N #. h) s :: N m s) of
      y -> M (\x -> k (m x y)) h m z
  postfixOf l (M k h m (z :: m)) s = reify (m, z) $
    \ (_ :: Proxy s) -> case runN (foldMapOf l (N #. h) s :: N m s) of
      y -> M (\x -> k (m x y)) h m z
  filtering p (M k h m z) = M k (\a -> if p a then h a else z) m z
  {-# INLINE run #-}
  {-# INLINE runOf #-}
  {-# INLINE prefix #-}
  {-# INLINE prefixOf #-}
  {-# INLINE postfix #-}
  {-# INLINE postfixOf #-}
  {-# INLINE filtering #-}

instance Profunctor M where
  dimap f g (M k h m e) = M (g.k) (h.f) m e
  {-# INLINE dimap #-}
  rmap g (M k h m e) = M (g.k) h m e
  {-# INLINE rmap #-}
  lmap f (M k h m e) = M k (h.f) m e
  {-# INLINE lmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Choice M where
  left' (M k h m z) = M (_Left %~ k) (_Left %~ h) step (Left z) where
    step (Left x) (Left y) = Left (m x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (M k h m z) = M (_Right %~ k) (_Right %~ h) step (Right z) where
    step (Right x) (Right y) = Right (m x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance Functor (M a) where
  fmap f (M k h m z) = M (f.k) h m z
  {-# INLINE fmap #-}

  (<$) b = \_ -> pure b
  {-# INLINE (<$) #-}

instance Comonad (M a) where
  extract (M k _ _ z) = k z
  {-# INLINE extract #-}

  duplicate (M k h m z) = M (\n -> M (k . m n) h m z) h m z
  {-# INLINE duplicate #-}

instance Applicative (M a) where
  pure b = M (\() -> b) (\_ -> ()) (\() () -> ()) ()
  {-# INLINE pure #-}

  M xf bx xx xz <*> M ya by yy yz = M
    (\(Pair' x y) -> xf x $ ya y)
    (\b -> Pair' (bx b) (by b))
    (\(Pair' x1 y1) (Pair' x2 y2) -> Pair' (xx x1 x2) (yy y1 y2))
    (Pair' xz yz)
  {-# INLINE (<*>) #-}

  (<*) m = \_ -> m
  {-# INLINE (<*) #-}

  _ *> m = m
  {-# INLINE (*>) #-}

instance Bind (M a) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad (M a) where
  return = pure
  {-# INLINE return #-}

  -- TODO: exploit observable sharing?
  m >>= f = M (\xs a -> run xs (f a)) One Two Zero <*> m
  {-# INLINE (>>=) #-}

  _ >> n = n
  {-# INLINE (>>) #-}

instance MonadZip (M a) where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

instance Extend (M a) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (M a) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

  (<.) m = \_ -> m
  {-# INLINE (<.) #-}

  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (M a) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

  (<@) m = \_ -> m
  {-# INLINE (<@) #-}

  _ @> m = m
  {-# INLINE (@>) #-}

instance Distributive (M a) where
  distribute fm = M (\t -> let g = foldDeRef t in run g <$> fm) One Two Zero
  {-# INLINE distribute #-}

instance Closed M where
  closed (M k h m z) = M (\f x -> k (f x)) (fmap h) (liftA2 m) (pure z)

instance Cosieve M FreeMonoid where
  cosieve = flip run

instance Profunctor.Corepresentable M where
  type Corep M = FreeMonoid
  cotabulate f = M (f . foldDeRef) One Two Zero

instance MonadReader (FreeMonoid a) (M a) where
  ask = askRep
  local = localRep

instance Functor.Representable (M a) where
  type Rep (M a) = FreeMonoid a
  tabulate = cotabulate
  index = cosieve

instance Costrong M where
  unfirst = unfirstCorep
  unsecond = unsecondCorep
