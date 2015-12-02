{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Fold.R
  ( R(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.Zip
import Data.Distributive
import Data.Foldable hiding (sum, product)
import Data.Fold.Class
import Data.Fold.Internal
import Data.Functor.Extend
import Data.Functor.Bind
import Data.Functor.Rep as Functor
import Data.Profunctor
import Data.Profunctor.Closed
import Data.Profunctor.Rep as Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Unsafe.Coerce
import Prelude hiding (foldr, sum, product, length)

-- | right folds / a reversed Moore machine
data R a b = forall r. R (r -> b) (a -> r -> r) r

instance Scan R where
  run1 t (R k h z)    = k (h t z)
  prefix1 a           = extend (run1 a)
  postfix1 t a        = run1 a (duplicate t)
  interspersing a (R k h z) = R (maybe' (k z) k) h' Nothing' where
    h' b Nothing'  = Just' (h b z)
    h' b (Just' x) = Just' (h b (h a x))
  {-# INLINE run1 #-}
  {-# INLINE prefix1 #-}
  {-# INLINE postfix1 #-}
  {-# INLINE interspersing #-}

-- | leaky 'prefix', efficient 'postfix'
instance Folding R where
  run t (R k h z)     = k (foldr h z t)
  runOf l s (R k h z) = k (foldrOf l h z s)
  prefix s            = extend (run s)
  prefixOf l s        = extend (runOf l s)
  postfix t s         = run s (duplicate t)
  postfixOf l t s     = runOf l s (duplicate t)
  filtering p (R k h z) = R k (\a r -> if p a then h a r else r) z
  {-# INLINE run #-}
  {-# INLINE runOf #-}
  {-# INLINE prefix #-}
  {-# INLINE prefixOf #-}
  {-# INLINE postfix #-}
  {-# INLINE postfixOf #-}
  {-# INLINE filtering #-}

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

  _ >> n = n
  {-# INLINE (>>) #-}

instance MonadZip (R a) where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

instance Applicative (R a) where
  pure b = R (\() -> b) (\_ () -> ()) ()
  {-# INLINE pure #-}

  R xf bxx xz <*> R ya byy yz = R
    (\(Pair' x y) -> xf x $ ya y)
    (\b ~(Pair' x y) -> Pair' (bxx b x) (byy b y))
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

instance Distributive (R a) where
  distribute = R (fmap extract) (fmap . prefix1)
  {-# INLINE distribute #-}

instance Functor.Representable (R a) where
  type Rep (R a) = [a]
  index = cosieve
  tabulate = cotabulate

instance Costrong R where
  unfirst = unfirstCorep
  unsecond = unsecondCorep

instance Profunctor.Corepresentable R where
  type Corep R = []
  cotabulate f = R (f . reverse) (:) []
  {-# INLINE cotabulate #-}

instance Cosieve R [] where
  cosieve (R k0 h0 z0) as0 = go k0 h0 z0 as0 where
    go k _ z [] = k z
    go k h z (a:as) = go k h (h a z) as
  {-# INLINE cosieve #-}

instance MonadReader [a] (R a) where
  ask = askRep
  local = localRep

instance Closed R where
  closed (R k h z) = R (\f x -> k (f x)) (liftA2 h) (pure z)
