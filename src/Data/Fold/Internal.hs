{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Fold.Internal
  ( SnocList(..)
  , SnocList1(..)
  , List1(..)
  , Maybe'(..), maybe'
  , Pair'(..)
  , N(..)
  , Tree(..)
  , An(..)
  , Box(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Foldable
import Data.Monoid hiding (First, Last)
import Data.Proxy
import Data.Reflection
import Data.Traversable

-- | Reversed '[]'
data SnocList a = Snoc (SnocList a) a | Nil
  deriving (Eq,Ord,Show,Read,Typeable,Data)

instance Functor SnocList where
  fmap f (Snoc xs x) = Snoc (fmap f xs) (f x)
  fmap _ Nil = Nil
  {-# INLINABLE fmap #-}

instance Foldable SnocList where
  foldl f z m0 = go m0 where
    go (Snoc xs x) = f (go xs) x
    go Nil = z
  {-# INLINE foldl #-}
  foldMap f (Snoc xs x) = foldMap f xs `mappend` f x
  foldMap _ Nil = mempty
  {-# INLINABLE foldMap #-}

instance Traversable SnocList where
  traverse f (Snoc xs x) = Snoc <$> traverse f xs <*> f x
  traverse _ Nil = pure Nil
  {-# INLINABLE traverse #-}

data SnocList1 a = Snoc1 (SnocList1 a) a | First a
  deriving (Eq,Ord,Show,Read,Typeable,Data)

instance Functor SnocList1 where
  fmap f (Snoc1 xs x) = Snoc1 (fmap f xs) (f x)
  fmap f (First a) = First (f a)
  {-# INLINABLE fmap #-}

instance Foldable SnocList1 where
  foldl f z m0 = go m0 where
    go (Snoc1 xs x) = f (go xs) x
    go (First a) = f z a
  {-# INLINE foldl #-}
  foldl1 f m0 = go m0 where
    go (Snoc1 xs x) = f (go xs) x
    go (First a) = a
  {-# INLINE foldl1 #-}
  foldMap f (Snoc1 xs x) = foldMap f xs `mappend` f x
  foldMap f (First a) = f a
  {-# INLINABLE foldMap #-}

instance Traversable SnocList1 where
  traverse f (Snoc1 xs x) = Snoc1 <$> traverse f xs <*> f x
  traverse f (First a) = First <$> f a
  {-# INLINABLE traverse #-}



-- | Strict 'Maybe'
data Maybe' a = Nothing' | Just' !a
  deriving (Eq,Ord,Show,Read,Typeable,Data)

instance Foldable Maybe' where
  foldMap _ Nothing' = mempty
  foldMap f (Just' a) = f a

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' _ f (Just' a) = f a
maybe' z _ Nothing'  = z
{-# INLINE maybe' #-}

-- | A reified 'Monoid'.
newtype N a s = N { runN :: a }
  deriving (Eq,Ord,Show,Read,Typeable,Data)

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
  deriving (Eq,Ord,Show,Read,Typeable,Data)

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
data Pair' a b = Pair' !a !b deriving (Eq,Ord,Show,Read,Typeable,Data)

instance (Monoid a, Monoid b) => Monoid (Pair' a b) where
  mempty = Pair' mempty mempty
  {-# INLINE mempty #-}
  mappend (Pair' a b) (Pair' c d) = Pair' (mappend a c) (mappend b d)
  {-# INLINE mappend #-}

newtype An a = An a deriving (Eq,Ord,Show,Read,Typeable,Data)

instance Functor An where
  fmap f (An a) = An (f a)

instance Foldable An where
  foldMap f (An a) = f a

instance Traversable An where
  traverse f (An a) = An <$> f a

data Box a = Box a deriving (Eq,Ord,Show,Read,Typeable,Data)

instance Functor Box where
  fmap f (Box a) = Box (f a)

instance Foldable Box where
  foldMap f (Box a) = f a

instance Traversable Box where
  traverse f (Box a) = Box <$> f a

data List1 a = Cons1 a (List1 a) | Last a

instance Functor List1 where
  fmap f (Cons1 a as) = Cons1 (f a) (fmap f as)
  fmap f (Last a) = Last (f a)

instance Foldable List1 where
  foldMap f = go where
    go (Cons1 a as) = f a `mappend` foldMap f as
    go (Last a) = f a
  {-# INLINE foldMap #-}

  foldr f z = go where
    go (Cons1 a as) = f a (go as)
    go (Last a) = f a z
  {-# INLINE foldr #-}

  foldr1 f = go where
    go (Cons1 a as) = f a (go as)
    go (Last a)     = a
  {-# INLINE foldr1 #-}

instance Traversable List1 where
  traverse f (Cons1 a as) = Cons1 <$> f a <*> traverse f as
  traverse f (Last a) = Last <$> f a
  {-# INLINABLE traverse #-}
