{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.Reducer
  ( Reducer(..)
  , reduce
  -- * Common reducers
  , reducer
  , monoid
  , sum
  , product
  , length
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Foldable hiding (sum, product)
import Data.Functor.Extend
import Data.Functor.Apply
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Reflection
import Unsafe.Coerce
import Prelude hiding (sum, product, length)

-- sequence algebras
data Reducer b a = forall m. Reducer (m -> a) (b -> m) (m -> m -> m) m

newtype M a s = M { runM :: a }

instance Reifies s (a -> a -> a, a) => Monoid (M a s) where
  mempty = M $ snd $ reflect (Proxy :: Proxy s)
  {-# INLINE mempty #-}
  mappend (M a) (M b) = M $ fst (reflect (Proxy :: Proxy s)) a b
  {-# INLINE mappend #-}

reduce :: Foldable t => t b -> Reducer b a -> a
reduce tb (Reducer k h m (e :: m)) = reify (m,e) $
  \ (_ :: Proxy s) -> k $ runM (foldMap (M #. h) tb :: M m s)

reduceOf :: Fold s b -> s -> Reducer b a -> a
reduceOf l s (Reducer k h m (e :: m)) = reify (m,e) $
  \ (_ :: Proxy s) -> k $ runM (foldMapOf l (M #. h) s :: M m s)

instance Profunctor Reducer where
  dimap f g (Reducer k h m e) = Reducer (g.k) (h.f) m e
  {-# INLINE dimap #-}
  rmap g (Reducer k h m e) = Reducer (g.k) h m e
  {-# INLINE rmap #-}
  lmap f (Reducer k h m e) = Reducer k (h.f) m e
  {-# INLINE lmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Choice Reducer where
  left' (Reducer k h m x) = Reducer (_Left %~ k) (_Left %~ h) step (Left x) where
    step (Left x) (Left y) = Left (m x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (Reducer k h m x) = Reducer (_Right %~ k) (_Right %~ h) step (Right x) where
    step (Right x) (Right y) = Right (m x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance Functor (Reducer a) where
  fmap f (Reducer k h m x) = Reducer (f.k) h m x
  {-# INLINE fmap #-}

  (<$) b = \_ -> pure b
  {-# INLINE (<$) #-}

instance Comonad (Reducer b) where
  extract (Reducer k _ _ x) = k x
  {-# INLINE extract #-}

  duplicate (Reducer k h m x) = Reducer (Reducer k h m) h m x
  {-# INLINE duplicate #-}

  extend f (Reducer k h m x)  = Reducer (f . Reducer k h m) h m x
  {-# INLINE extend #-}

data Pair a b = Pair !a !b

instance Applicative (Reducer b) where
  pure b = Reducer (\() -> b) (\_ -> ()) (\() () -> ()) ()
  {-# INLINE pure #-}

  Reducer xf bx xx x <*> Reducer ya by yy y = Reducer
    (\(Pair x y) -> xf x $ ya y)
    (\b -> Pair (bx b) (by b))
    (\(Pair x1 y1) (Pair x2 y2) -> Pair (xx x1 x2) (yy y1 y2))
    (Pair x y)
  {-# INLINE (<*>) #-}

  (<*) m = \_ -> m
  {-# INLINE (<*) #-}

  _ *> m = m
  {-# INLINE (*>) #-}

instance Extend (Reducer b) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (Reducer b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

  (<.) m = \_ -> m
  {-# INLINE (<.) #-}

  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (Reducer b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

  (<@) m = \_ -> m
  {-# INLINE (<@) #-}

  _ @> m = m
  {-# INLINE (@>) #-}

-- * Common reducers

reducer :: (m -> m -> m) -> m -> Reducer m m
reducer = Reducer id id
{-# INLINE reducer #-}

monoid :: Monoid m => Reducer m m
monoid = reducer mappend mempty
{-# INLINE monoid #-}

sum :: Num a => Reducer a a
sum = reducer (+) 0
{-# INLINE sum #-}

product :: Num a => Reducer a a
product = reducer (*) 1
{-# INLINE product #-}

length :: Reducer a Int
length = Reducer id (\_ -> 1) (+) 0
{-# INLINE length #-}

genericLength :: Num a => Reducer b a
genericLength = Reducer id (\_ -> 1) (+) 0
{-# INLINE genericLength #-}
