{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.L
  ( L(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Foldable
import Data.Fold.Class
import Data.Functor.Extend
import Data.Functor.Apply
import Data.Profunctor.Unsafe
import Data.Traversable
import Unsafe.Coerce
import Prelude hiding (foldl)

-- left folds
data L b a = forall r. L (r -> a) (r -> b -> r) r

instance Folding L where
  enfold t (L k h z)      = k (foldl h z t)
  enfoldOf l s (L k h z)  = k (foldlOf l h z s)
  enfold' t (L k h z)     = k (foldl' h z t)
  enfoldOf' l s (L k h z) = k (foldlOf' l h z s)
  enscan s (L k h z) = snd (mapAccumL h' z s) where
    h' r a = (r', k r') where r' = h r a
  enscanOf l s (L k h z) = snd (mapAccumLOf l h' z s) where
    h' r a = (r', k r') where r' = h r a


instance Profunctor L where
  dimap f g (L k h z) = L (g.k) (\r -> h r . f) z
  {-# INLINE dimap #-}
  rmap g (L k h z) = L (g.k) h z
  {-# INLINE rmap #-}
  lmap f (L k h z) = L k (\r -> h r . f) z
  {-# INLINE lmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Choice L where
  left' (L k h z) = L (_Left %~ k) step (Left z) where
    step (Left x) (Left y) = Left (h x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (L k h z) = L (_Right %~ k) step (Right z) where
    step (Right x) (Right y) = Right (h x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance Functor (L a) where
  fmap f (L k h z) = L (f.k) h z
  {-# INLINE fmap #-}

  (<$) b = \_ -> pure b
  {-# INLINE (<$) #-}

instance Comonad (L b) where
  extract (L k _ z) = k z
  {-# INLINE extract #-}

  duplicate (L k h z) = L (L k h) h z
  {-# INLINE duplicate #-}

  extend f (L k h z)  = L (f . L k h) h z
  {-# INLINE extend #-}

data Pair a b = Pair !a !b

instance Applicative (L b) where
  pure b = L (\() -> b) (\() _ -> ()) ()
  {-# INLINE pure #-}

  L xf bxx xz <*> L ya byy yz = L
    (\(Pair x y) -> xf x $ ya y)
    (\(Pair x y) b -> Pair (bxx x b) (byy y b))
    (Pair xz yz)
  {-# INLINE (<*>) #-}

  (<*) m = \_ -> m
  {-# INLINE (<*) #-}

  _ *> m = m
  {-# INLINE (*>) #-}

instance Extend (L b) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (L b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

  (<.) m = \_ -> m
  {-# INLINE (<.) #-}

  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (L b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

  (<@) m = \_ -> m
  {-# INLINE (<@) #-}

  _ @> m = m
  {-# INLINE (@>) #-}
