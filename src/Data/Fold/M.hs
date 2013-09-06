{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Unlike 'Data.Fold.L' and 'Data.Fold.R' this 'Comonad'
-- is based on a @(->) r@ 'Comonad' for a 'Monoid' @r@ rather than
-- than on a 'Store r'.
module Data.Fold.M
  ( M(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Fold.Class
import Data.Foldable hiding (sum, product)
import Data.Functor.Extend
import Data.Functor.Bind
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Reflection
import Unsafe.Coerce
import Prelude hiding (sum, product, length)

-- | A 'foldMap' caught in amber.
data M b a = forall m. M (m -> a) (b -> m) (m -> m -> m) m

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

instance Comonad (M b) where
  extract (M k _ _ z) = k z
  {-# INLINE extract #-}

  duplicate (M k h m z) = M (\n -> M (k . m n) h m z) h m z
  {-# INLINE duplicate #-}

data Pair a b = Pair !a !b

instance Applicative (M b) where
  pure b = M (\() -> b) (\_ -> ()) (\() () -> ()) ()
  {-# INLINE pure #-}

  M xf bx xx xz <*> M ya by yy yz = M
    (\(Pair x y) -> xf x $ ya y)
    (\b -> Pair (bx b) (by b))
    (\(Pair x1 y1) (Pair x2 y2) -> Pair (xx x1 x2) (yy y1 y2))
    (Pair xz yz)
  {-# INLINE (<*>) #-}

  (<*) m = \_ -> m
  {-# INLINE (<*) #-}

  _ *> m = m
  {-# INLINE (*>) #-}

instance Bind (M b) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad (M b) where
  return = pure
  {-# INLINE return #-}
  m >>= f = M (\xs a -> run xs (f a)) One Two Zero <*> m
  {-# INLINE (>>=) #-}

instance Extend (M b) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (M b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

  (<.) m = \_ -> m
  {-# INLINE (<.) #-}

  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (M b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

  (<@) m = \_ -> m
  {-# INLINE (<@) #-}

  _ @> m = m
  {-# INLINE (@>) #-}

-- * Internals

-- | A reified 'Monoid'.
newtype N a s = N { runN :: a }

instance Reifies s (a -> a -> a, a) => Monoid (N a s) where
  mempty = N $ snd $ reflect (Proxy :: Proxy s)
  {-# INLINE mempty #-}
  mappend (N a) (N b) = N $ fst (reflect (Proxy :: Proxy s)) a b
  {-# INLINE mappend #-}

-- | The shape of a 'foldMap'
data Tree a = Zero | One a | Two (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Zero = Zero
  fmap f (One a) = One (f a)
  fmap f (Two a b) = Two (fmap f a) (fmap f b)

instance Foldable Tree where
  foldMap _ Zero = mempty
  foldMap f (One a) = f a
  foldMap f (Two a b) = foldMap f a `mappend` foldMap f b

instance Traversable Tree where
  traverse _ Zero = pure Zero
  traverse f (One a) = One <$> f a
  traverse f (Two a b) = Two <$> traverse f a <*> traverse f b

