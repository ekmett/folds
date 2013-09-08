{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Fold.Internal
  ( SnocList(..)
  , Maybe'(..), maybe'
  , Pair'(..)
  , N(..)
  , Tree(..)
  ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Proxy
import Data.Reflection
import Data.Traversable

-- | Reversed '[]'
data SnocList a = Snoc (SnocList a) a | Nil

instance Foldable SnocList where
  foldl f z m0 = go m0 where
    go (Snoc xs x) = f (go xs) x
    go Nil = z
  {-# INLINE foldl #-}
  foldMap f (Snoc xs x) = foldMap f xs `mappend` f x
  foldMap _ Nil = mempty
  {-# INLINE foldMap #-}

-- | Strict 'Maybe'
data Maybe' a = Nothing' | Just' !a

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' _ f (Just' a) = f a
maybe' z _ Nothing'  = z
{-# INLINE maybe' #-}

-- | A reified 'Monoid'.
newtype N a s = N { runN :: a }

instance Reifies s (a -> a -> a, a) => Monoid (N a s) where
  mempty = N $ snd $ reflect (Proxy :: Proxy s)
  {-# INLINE mempty #-}
  mappend (N a) (N b) = N $ fst (reflect (Proxy :: Proxy s)) a b
  {-# INLINE mappend #-}

-- | The shape of a 'foldMap'
data Tree a
  = Zero
  | One a
  | Two (Tree a) (Tree a)

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

-- | Strict Pair
data Pair' a b = Pair' !a !b
