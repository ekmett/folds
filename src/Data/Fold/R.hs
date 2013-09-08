{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.R
  ( R(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Foldable hiding (sum, product)
import Data.Fold.Class
import Data.Fold.Internal
import Data.Functor.Extend
import Data.Functor.Bind
import Data.Profunctor.Unsafe
import Unsafe.Coerce
import Prelude hiding (foldr, sum, product, length)

-- right folds
data R a b = forall r. R (r -> b) (a -> r -> r) r

-- | leaky 'prefix', efficient 'postfix'
instance Folding R where
  run t (R k h z)     = k (foldr h z t)
  run1 t (R k h z)    = k (h t z)
  runOf l s (R k h z) = k (foldrOf l h z s)
  prefix s            = extend (run s)
  prefix1 a           = extend (run1 a)
  prefixOf l s        = extend (runOf l s)
  postfix t s         = run s (duplicate t)
  postfix1 t a        = run1 a (duplicate t)
  postfixOf l t s     = runOf l s (duplicate t)
  filtering p (R k h z) = R k (\a r -> if p a then h a r else r) z
  interspersing a (R k h z) = R (maybe' (k z) k) h' Nothing' where
    h' b Nothing'  = Just' (h b z)
    h' b (Just' x) = Just' (h b (h a x))
  {-# INLINE run #-}
  {-# INLINE run1 #-}
  {-# INLINE runOf #-}
  {-# INLINE prefix #-}
  {-# INLINE prefix1 #-}
  {-# INLINE prefixOf #-}
  {-# INLINE postfix #-}
  {-# INLINE postfix1 #-}
  {-# INLINE postfixOf #-}
  {-# INLINE filtering #-}
  {-# INLINE interspersing #-}

instance Profunctor R where
  dimap f g (R k h z) = R (g.k) (h.f) z
  {-# INLINE dimap #-}
  rmap g (R k h z) = R (g.k) h z
  {-# INLINE rmap #-}
  lmap f (R k h z) = R k (h.f) z
  {-# INLINE lmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}
  x .# _ = unsafeCoerce x
  {-# INLINE (.#) #-}

instance Choice R where
  left' (R k h z) = R (_Left %~ k) step (Left z) where
    step (Left x) (Left y) = Left (h x y)
    step (Right c) _ = Right c
    step _ (Right c) = Right c
  {-# INLINE left' #-}

  right' (R k h z) = R (_Right %~ k) step (Right z) where
    step (Right x) (Right y) = Right (h x y)
    step (Left c) _ = Left c
    step _ (Left c) = Left c
  {-# INLINE right' #-}

instance Functor (R a) where
  fmap f (R k h z) = R (f.k) h z
  {-# INLINE fmap #-}

  (<$) b = \_ -> pure b
  {-# INLINE (<$) #-}

instance Comonad (R a) where
  extract (R k _ z) = k z
  {-# INLINE extract #-}

  duplicate (R k h z) = R (R k h) h z
  {-# INLINE duplicate #-}

  extend f (R k h z)  = R (f . R k h) h z
  {-# INLINE extend #-}

instance Bind (R a) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad (R a) where
  return b = R (\() -> b) (\_ () -> ()) ()
  {-# INLINE return #-}

  m >>= f = R (\xs a -> run xs (f a)) (:) [] <*> m
  {-# INLINE (>>=) #-}

instance Applicative (R a) where
  pure b = R (\() -> b) (\_ () -> ()) ()
  {-# INLINE pure #-}

  R xf bxx xz <*> R ya byy yz = R
    (\(Pair' x y) -> xf x $ ya y)
    (\b (Pair' x y) -> Pair' (bxx b x) (byy b y))
    (Pair' xz yz)
  {-# INLINE (<*>) #-}

  (<*) m = \_ -> m
  {-# INLINE (<*) #-}

  _ *> m = m
  {-# INLINE (*>) #-}

instance Extend (R a) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (R a) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

  (<.) m = \_ -> m
  {-# INLINE (<.) #-}

  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (R a) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

  (<@) m = \_ -> m
  {-# INLINE (<@) #-}

  _ @> m = m
  {-# INLINE (@>) #-}
