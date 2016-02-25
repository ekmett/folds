{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Fold.Internal
  ( SnocList(..)
  , SnocList1(..)
  , List1(..)
  , Maybe'(..), maybe'
  , Pair'(..)
  , N(..)
  , S(..)
  , Tree(..)
  , Tree1(..)
  , An(..)
  , Box(..)
  , FreeMonoid(..)
  , foldDeRef
  , FreeSemigroup(..)
  , foldDeRef1
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Fix
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Constraint
import Data.Data (Data, Typeable)
import Data.Semigroup hiding (Last, First)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif
import Data.Functor.Bind
import Data.HashMap.Lazy as HM
import Data.Profunctor.Unsafe
import Data.Proxy (Proxy(Proxy))
import Data.Reflection
import Data.Reify
import Data.Semigroup.Foldable
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import System.IO.Unsafe

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

instance Reifies s (a -> a -> a, a) => Semigroup (N a s) where
  N a <> N b = N $ fst (reflect (Proxy :: Proxy s)) a b
  {-# INLINE (<>) #-}

instance Reifies s (a -> a -> a, a) => Monoid (N a s) where
  mempty = N $ snd $ reflect (Proxy :: Proxy s)
  {-# INLINE mempty #-}
  mappend = (<>)
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

-- | A reified 'Semigroup'.
newtype S a s = S { runS :: a }
  deriving (Eq,Ord,Show,Read,Typeable,Data)

instance Reifies s (a -> a -> a) => Semigroup (S a s) where
  S a <> S b = S $ reflect (Proxy :: Proxy s) a b

-- | Strict Pair
data Pair' a b = Pair' !a !b deriving (Eq,Ord,Show,Read,Typeable,Data)

instance (Semigroup a, Semigroup b) => Semigroup (Pair' a b) where
  Pair' a b <> Pair' c d = Pair' (a <> c) (b <> d)
  {-# INLINE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (Pair' a b) where
  mempty = Pair' mempty mempty
  {-# INLINE mempty #-}

  -- TODO/FIXME: Once Semigroup becomes a superclass
  -- `#if MIN_VERSION_base(...)`-out this definition
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

data Tree1 a = Bin1 (Tree1 a) (Tree1 a) | Tip1 a

instance Functor Tree1 where
  fmap f (Bin1 as bs) = Bin1 (fmap f as) (fmap f bs)
  fmap f (Tip1 a) = Tip1 (f a)

instance Foldable Tree1 where
  foldMap f (Bin1 as bs) = foldMap f as `mappend` foldMap f bs
  foldMap f (Tip1 a) = f a

instance Traversable Tree1 where
  traverse f (Bin1 as bs) = Bin1 <$> traverse f as <*> traverse f bs
  traverse f (Tip1 a) = Tip1 <$> f a

newtype FreeMonoid a = FreeMonoid { runFreeMonoid :: forall m. Monoid m => (a -> m) -> m } deriving Functor

instance Foldable FreeMonoid where
  foldMap f m = runFreeMonoid m f

newtype FreeSemigroup a = FreeSemigroup { runFreeSemigroup :: forall m. Semigroup m => (a -> m) -> m } deriving Functor

instance Foldable FreeSemigroup where
  foldMap f (FreeSemigroup m) = unwrapMonoid $ m (WrapMonoid #. f)

instance Foldable1 FreeSemigroup where
  foldMap1 f (FreeSemigroup m) = m f

data T a b = T0 | T1 a | T2 b b deriving (Functor, Foldable, Traversable)

instance MuRef (Tree a) where
  type DeRef (Tree a) = T a
  mapDeRef _ Zero    = pure T0
  mapDeRef _ (One a) = pure (T1 a)
  mapDeRef f (Two x y) = T2 <$> f x <*> f y

class MuRef1 (f :: * -> *) where
  type DeRef1 f :: * -> * -> *
  muRef1 :: proxy (f a) -> Dict (MuRef (f a), DeRef (f a) ~ DeRef1 f a)

foldDeRef :: forall f a. (MuRef1 f, Bifoldable (DeRef1 f)) => f a -> FreeMonoid a
foldDeRef m = case muRef1 (undefined :: Proxy (f a)) of
  Dict -> case unsafePerformIO (reifyGraph m) of
    Graph xs i | hm <- HM.fromList xs -> FreeMonoid $ \ f -> fix (\mm -> fmap (bifoldMap f (mm !)) hm) ! i

instance MuRef1 Tree where
  type DeRef1 Tree = T
  muRef1 _ = Dict

instance Bifunctor T where
  bimap _ _ T0 = T0
  bimap f _ (T1 a) = T1 (f a)
  bimap _ g (T2 b c) = T2 (g b) (g c)

instance Bifoldable T where
  bifoldMap _ _ T0 = mempty
  bifoldMap f _ (T1 a) = f a
  bifoldMap _ g (T2 b c) = g b `mappend` g c

instance Bitraversable T where
  bitraverse _ _ T0 = pure T0
  bitraverse f _ (T1 a) = T1 <$> f a
  bitraverse _ g (T2 b c) = T2 <$> g b <*> g c

data T1 a b = A a | B b b deriving (Functor, Foldable, Traversable)

instance MuRef (Tree1 a) where
  type DeRef (Tree1 a) = T1 a
  mapDeRef _ (Tip1 a) = pure (A a)
  mapDeRef f (Bin1 b c) = B <$> f b <*> f c

instance MuRef1 Tree1 where
  type DeRef1 Tree1 = T1
  muRef1 _ = Dict

instance Bifunctor T1 where
  bimap f _ (A a) = A (f a)
  bimap _ g (B b c) = B (g b) (g c)

instance Bifoldable T1 where
  bifoldMap f _ (A a) = f a
  bifoldMap _ g (B b c) = g b `mappend` g c

instance Bitraversable T1 where
  bitraverse f _ (A a) = A <$> f a
  bitraverse _ g (B b c) = B <$> g b <*> g c

foldDeRef1 :: forall f a. (MuRef1 f, Bifoldable1 (DeRef1 f)) => f a -> FreeSemigroup a
foldDeRef1 m = case muRef1 (undefined :: Proxy (f a)) of
  Dict -> case unsafePerformIO (reifyGraph m) of
    Graph xs i | hm <- HM.fromList xs -> FreeSemigroup $ \ f -> fix (\mm -> fmap (bifoldMap1 f (mm !)) hm) ! i

instance Bifoldable1 T1 where
  bifoldMap1 f _ (A a) = f a
  bifoldMap1 _ g (B b c) = g b <> g c

instance Bitraversable1 T1 where
  bitraverse1 f _ (A a) = A <$> f a
  bitraverse1 _ g (B b c) = B <$> g b <.> g c
