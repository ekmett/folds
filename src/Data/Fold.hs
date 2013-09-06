module Data.Fold
  ( Folding(..)
  , L(..)
  , M(..)
  , R(..)
  , r2m
  , m2r
  , l2m
  ) where

import Data.Fold.Class
import Data.Fold.L
import Data.Fold.M
import Data.Fold.R
import Control.Category ((>>>))

-- |
--
-- 'foldr' and 'foldMap' are equivalent in expressive power for finite folds.
r2m :: R a b -> M a b
r2m (R k h z) = M (\f -> k (f z)) h (.) id

-- |
--
-- We can convert from a monoidal fold to a right fold.
--
-- @
-- run xs (m2r l) = run xs l
-- @
m2r :: M a b -> R a b
m2r (M k h m z) = R k (m . h) z

-- |
--
-- We can convert from a left fold to a monoidal fold.
--
-- @
-- run xs (l2m l) = run xs l
-- @
l2m :: L a b -> M a b
l2m (L k h z) = M (\f -> k (f z)) (flip h) (>>>) id
