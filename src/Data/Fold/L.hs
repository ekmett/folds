{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Fold.L
  ( L(..)
  , unfoldL
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Zip
import Data.Foldable
import Data.Fold.Class
import Data.Fold.Internal
import Data.Functor.Extend
import Data.Functor.Bind
import Data.Profunctor.Unsafe
import Unsafe.Coerce
import Prelude hiding (foldl)

-- | A Moore Machine
data L a b = forall r. L (r -> b) (r -> a -> r) r

-- | Construct a Moore machine from a state valuation and transition function
unfoldL :: (s -> (b, a -> s)) -> s -> L a b
unfoldL f = L (fst . f) (snd . f)
{-# INLINE unfoldL #-}

instance Scan L where
  run1 t (L k h z)    = k (h z t)
  prefix1 a           = run1 a . duplicate
  postfix1 t a        = extend (run1 a) t
  interspersing a (L k h z) = L (maybe' (k z) k) h' Nothing' where
    h' Nothing' b  = Just' (h z b)
    h' (Just' x) b = Just' (h (h x a) b)
  {-# INLINE run1 #-}
  {-# INLINE prefix1 #-}
  {-# INLINE postfix1 #-}
  {-# INLINE interspersing #-}

-- | efficient 'prefix', leaky 'postfix'
instance Folding L where
  run t (L k h z)     = k (foldl h z t)
  runOf l s (L k h z) = k (foldlOf l h z s)
  prefix s            = run s . duplicate
  prefixOf l s        = runOf l s . duplicate
  postfix t s         = extend (run s) t
  postfixOf l t s     = extend (runOf l s) t
  filtering p (L k h z) = L k (\r a -> if p a then h r a else r) z
  {-# INLINE run #-}
  {-# INLINE runOf #-}
  {-# INLINE prefix #-}
  {-# INLINE prefixOf #-}
  {-# INLINE postfix #-}
  {-# INLINE postfixOf #-}
  {-# INLINE filtering #-}

{-
enscanl s (L k h z) = snd (mapAccumL h' z s) where
  h' r a = (r', k r') where r' = h r a

enscanlOf l s (L k h z) = snd (mapAccumLOf l h' z s) where
  h' r a = (r', k r') where r' = h r a
-}

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

instance Comonad (L a) where
  extract (L k _ z) = k z
  {-# INLINE extract #-}

  duplicate (L k h z) = L (L k h) h z
  {-# INLINE duplicate #-}

  extend f (L k h z)  = L (f . L k h) h z
  {-# INLINE extend #-}

instance Applicative (L a) where
  pure b = L (\() -> b) (\() _ -> ()) ()
  {-# INLINE pure #-}

  L xf bxx xz <*> L ya byy yz = L
    (\(Pair' x y) -> xf x $ ya y)
    (\(Pair' x y) b -> Pair' (bxx x b) (byy y b))
    (Pair' xz yz)
  {-# INLINE (<*>) #-}

  (<*) m = \_ -> m
  {-# INLINE (<*) #-}

  _ *> m = m
  {-# INLINE (*>) #-}

instance Bind (L a) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad (L a) where
  return = pure
  {-# INLINE return #-}

  m >>= f = L (\xs a -> run xs (f a)) Snoc Nil <*> m
  {-# INLINE (>>=) #-}

  _ >> n = n
  {-# INLINE (>>) #-}

instance MonadZip (L a) where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

instance Extend (L a) where
  extended = extend
  {-# INLINE extended #-}

  duplicated = duplicate
  {-# INLINE duplicated #-}

instance Apply (L a) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

  (<.) m = \_ -> m
  {-# INLINE (<.) #-}

  _ .> m = m
  {-# INLINE (.>) #-}

instance ComonadApply (L a) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

  (<@) m = \_ -> m
  {-# INLINE (<@) #-}

  _ @> m = m
  {-# INLINE (@>) #-}
