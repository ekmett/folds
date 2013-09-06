module Data.Fold
  ( Folding(..)
  -- * Foldings
  , L(..)
  , M(..)
  , R(..)
  -- * Folding Homomorphisms
  -- $hom
  , r2m
  , m2r
  , l2m
  , l2r
  ) where

import Data.Fold.Class
import Data.Fold.L
import Data.Fold.M
import Data.Fold.R
import Control.Category ((>>>))

-- * Folding Homomorphisms

-- $hom
--
-- We define @f@ to be a folding homomorphism betwen @p@ and @q@ when:
--
-- @
-- f :: forall a b. p a b -> q a b
-- @
--
-- @
-- 'run' xs (f m)     ≡ 'run' xs m
-- 'prefix' xs (f m)  ≡ f ('prefix' xs m)
-- 'postfix' (f m) xs ≡ f ('postfix' m xs)
-- @

-- | @r2m@ is a folding homomorphism between right and monoidal foldings
--
-- @
-- run xs (r2m r)     ≡ run xs r
-- prefix xs (r2m r)  ≡ r2m (prefix xs r)
-- postfix (r2m r) xs ≡ r2m (postfix r xs)
-- @
--
-- 'foldr' and 'foldMap' are equivalent in expressive power for finite foldings.
--
-- For all right folds:
--
-- @
-- m2r.r2m = id
-- @
--
-- For legal monoidal folds, applied to structures that only require finite
-- reassociation:
--
-- @
-- r2m.m2r = id
-- @
r2m :: R a b -> M a b
r2m (R k h z) = M (\f -> k (f z)) h (.) id

-- | @m2r@ is a folding homomorphism between monoidal and right foldings
--
-- We can convert from a monoidal folding to a right folding.
--
-- @
-- run xs (m2r m)     ≡ run xs m
-- prefix xs (m2r m)  ≡ m2r (prefix xs m)
-- postfix (m2r m) xs ≡ m2r (postfix m xs)
-- @
m2r :: M a b -> R a b
m2r (M k h m z) = R k (m . h) z

-- |
--
-- We can convert from a left folding to a monoidal folding.
--
-- @
-- run xs (l2m l) ≡ run xs l
-- prefix xs (l2m l) ≡ l2m (prefix xs l)
-- @
l2m :: L a b -> M a b
l2m (L k h z) = M (\f -> k (f z)) (flip h) (>>>) id

-- |
--
-- We can convert from a left folding to a right folding.
--
-- @
-- run xs (l2r l) ≡ run xs l
-- prefix xs (l2r l) ≡ l2r (prefix xs l)
-- @
l2r :: L a b -> R a b
l2r (L k h z) = R (\f -> k (f z)) (\b g x -> g (h x b)) id
