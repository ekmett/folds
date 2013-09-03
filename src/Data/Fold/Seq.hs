{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.Seq
  ( Seq(..)
  , sequenced
  , Tree(..)
  , tree
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Foldable
import Data.Functor.Extend
import Data.Functor.Apply
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Traversable
import Unsafe.Coerce

-- | A sequence algebra
data Seq b a = forall x. Seq (x -> a) (x -> b -> x -> x) x

instance Profunctor Seq where
  dimap f g (Seq xb h x) = Seq (g.xb) (\x a -> h x (f a)) x
  {-# INLINE dimap #-}
  rmap g (Seq xb h x) = Seq (g.xb) h x
  {-# INLINE rmap #-}
  lmap f (Seq xb h x) = Seq xb (\x a -> h x (f a)) x
  {-# INLINE lmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Choice Seq where
  left' (Seq k h x) = Seq (_Left %~ k) step (Left x) where
    step (Left x) (Left a) (Left y) = Left (h x a y)
    step (Right c) _ _ = Right c
    step _ (Right c) _ = Right c
    step _ _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (Seq k h x) = Seq (_Right %~ k) step (Right x) where
    step (Right x) (Right a) (Right y) = Right (h x a y)
    step (Left c) _ _ = Left c
    step _ (Left c) _ = Left c
    step _ _ (Left c) = Left c
  {-# INLINE right' #-}

instance Functor (Seq a) where
  fmap f (Seq k h x) = Seq (f.k) h x
  {-# INLINE fmap #-}

  (<$) b = \_ -> pure b
  {-# INLINE (<$) #-}

instance Comonad (Seq b) where
  extract (Seq k _ x) = k x
  {-# INLINE extract #-}
  duplicate (Seq k h x) = Seq (Seq k h) h x
  {-# INLINE duplicate #-}
  extend f (Seq k h x)  = Seq (f . Seq k h) h x
  {-# INLINE extend #-}

data Pair a b = Pair !a !b

instance Applicative (Seq b) where
  pure b = Seq (\() -> b) (\() _ () -> ()) ()
  {-# INLINE pure #-}

  Seq xf xm x <*> Seq ya ym y = Seq
    (\(Pair x y) -> xf x $ ya y)
    (\(Pair x1 y1) b (Pair x2 y2) -> Pair (xm x1 b x2) (ym y1 b y2))
    (Pair x y)
  {-# INLINE (<*>) #-}
  (<*) m = \_ -> m
  {-# INLINE (<*) #-}
  _ *> m = m
  {-# INLINE (*>) #-}

instance Extend (Seq b) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (Seq b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
  (<.) m = \_ -> m
  {-# INLINE (<.) #-}
  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (Seq b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}
  (<@) m = \_ -> m
  {-# INLINE (<@) #-}
  _ @> m = m
  {-# INLINE (@>) #-}

-- | The carrier for the initial sequence algebra
data Tree a
  = Tip
  | Bin (Tree a) a (Tree a)

instance Functor Tree where
  fmap f t0 = go t0 where
    go Tip = Tip
    go (Bin l a r) = Bin (go l) (f a) (go r)
  {-# INLINE fmap #-}

instance Foldable Tree where
  foldMap f t0 = go t0 where
    go Tip = mempty
    go (Bin l a r) = go l `mappend` f a `mappend` go r
  {-# INLINE foldMap #-}

instance Traversable Tree where
  traverse f t0 = go t0 where
    go Tip = pure Tip
    go (Bin l a r) = Bin <$> go l <*> f a <*> go r
  {-# INLINE traverse #-}

-- | the initial sequence algebra
tree :: Seq a (Tree a)
tree = Seq id Bin Tip
{-# INLINE tree #-}

-- | apply a sequence algebra
sequenced :: Tree b -> Seq b a -> a
sequenced t0 (Seq k h x) = k (go t0) where
  go Tip = x
  go (Bin l a r) = h (go l) a (go r)
{-# INLINE sequenced #-}
