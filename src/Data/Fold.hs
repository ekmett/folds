module Data.Fold
  ( Folding(..)
  -- * Foldings
  , L(..)
  , M(..)
  , R(..)
  -- * Folding Homomorphisms
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

-- |
--
-- 'foldr' and 'foldMap' are equivalent in expressive power for finite foldings.
--
-- @
-- run xs (r2m r) ≡ run xs r
-- @
r2m :: R a b -> M a b
r2m (R k h z) = M (\f -> k (f z)) h (.) id

-- |
--
-- We can convert from a monoidal folding to a right folding.
--
-- @
-- run xs (m2r m) ≡ run xs m
-- @
m2r :: M a b -> R a b
m2r (M k h m z) = R k (m . h) z

-- |
--
-- We can convert from a left folding to a monoidal folding.
--
-- @
-- run xs (l2m l) = run xs l
-- @
l2m :: L a b -> M a b
l2m (L k h z) = M (\f -> k (f z)) (flip h) (>>>) id

-- |
--
-- We can convert from a left folding to a right folding.
--
-- @
-- run xs (l2r l) = run xs l
-- @
l2r :: L a b -> R a b
l2r (L k h z) = R (\f -> k (f z)) (\b g x -> g (h x b)) id
