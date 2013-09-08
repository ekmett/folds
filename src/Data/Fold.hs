-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2009-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Fold
  (
  -- * Foldings
  -- ** Left Foldings
    L(..), L'(..)
  -- ** Monoidal Foldings
  , M(..)
  -- ** Right Foldings
  , R(..)
  -- * Folding Combinators
  , Folding(..)
  , beneath
  -- * Folding Homomorphisms
  -- $hom
  , AsRM(..)
  , AsL'(..)
  ) where

import Data.Fold.Class
import Data.Fold.L
import Data.Fold.L'
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
-- 'run' xs (f φ)         ≡ 'run' xs φ
-- 'runOf' l xs (f φ)     ≡ 'runOf' l xs φ
-- 'prefix' xs (f φ)      ≡ f ('prefix' xs φ)
-- 'prefixOf' l xs (f φ)  ≡ f ('prefixOf' l xs φ)
-- 'postfix' (f φ) xs     ≡ f ('postfix' φ xs)
-- 'postfixOf' l (f φ) xs ≡ f ('postfixOf' l φ xs)
-- 'left'' (f φ)          ≡ f ('left'' φ)
-- 'right'' (f φ)         ≡ f ('right'' φ)
-- 'dimap' l r (f φ)      ≡ f ('dimap' l r φ)
-- 'extract' (f φ)        ≡ 'extract' φ
-- 'pure' a               ≡ f ('pure' a)
-- f φ '<*>' f ψ          ≡ f (φ '<*>' ψ)
-- 'return' a             ≡ f ('return' a)
-- f φ '>>=' f . k        ≡ f (φ '>>=' k)
-- @
--
-- Note: A law including 'extend' is explicitly excluded.

class AsRM p where
  -- | 'asM' is a folding homomorphism to a monoidal folding
  --
  -- @
  -- 'run' xs ('asM' φ)         ≡ 'run' xs φ
  -- 'prefix' xs ('asM' φ)      ≡ 'asM' ('prefix' xs φ)
  -- 'prefixOf' l xs ('asM' φ)  ≡ 'asM' ('prefixOf' l xs φ)
  -- 'postfix' ('asM' φ) xs     ≡ 'asM' ('postfix' φ xs)
  -- 'postfixOf' l ('asM' φ) xs ≡ 'asM' ('postfixOf' l φ xs)
  -- 'left'' ('asM' φ)          ≡ 'asM' ('left'' φ)
  -- 'right'' ('asM' φ)         ≡ 'asM' ('right'' φ)
  -- 'dimap' l r ('asM' φ)      ≡ 'asM' ('dimap' l r φ)
  -- 'extract' ('asM' φ)        ≡ 'extract' φ
  -- 'pure' a                  ≡ 'asM' ('pure' a)
  -- 'asM' φ '<*>' 'asM' ψ        ≡ 'asM' (φ '<*>' ψ)
  -- 'return' a                ≡ 'asM' ('return' a)
  -- 'asM' φ '>>=' 'asM' . k      ≡ 'asM' (φ '>>=' k)
  -- @
  asM :: p a b -> M a b
  asM = asM . asR

  -- | 'asR' is a folding homomorphism to a right folding
  --
  -- @
  -- 'run' xs ('asR' φ)         ≡ 'run' xs φ
  -- 'prefix' xs ('asR' φ)      ≡ 'asR' ('prefix' xs φ)
  -- 'prefixOf' l xs ('asR' φ)  ≡ 'asR' ('prefixOf' l xs φ)
  -- 'postfix' ('asR' φ) xs     ≡ 'asR' ('postfix' φ xs)
  -- 'postfixOf' l ('asR' φ) xs ≡ 'asR' ('postfixOf' l φ xs)
  -- 'left'' ('asR' φ)          ≡ 'asR' ('left'' φ)
  -- 'right'' ('asR' φ)         ≡ 'asR' ('right'' φ)
  -- 'dimap' l r ('asR' φ)      ≡ 'asR' ('dimap' l r φ)
  -- 'extract' ('asR' φ)        ≡ 'extract' φ
  -- 'pure' a                  ≡ 'asR' ('pure' a)
  -- 'asR' φ '<*>' 'asR' ψ        ≡ 'asR' (φ '<*>' ψ)
  -- 'return' a                ≡ 'asR' ('return' a)
  -- 'asR' φ '>>=' 'asR' . k      ≡ 'asR' (φ '>>=' k)
  -- @
  asR :: p a b -> R a b
  asR = asR . asM

-- | We can convert from a lazy right fold to a monoidal fold
instance AsRM R where
  asM (R k h z) = M (\f -> k (f z)) h (.) id
  asR = id

-- | We can convert from a monoidal fold to a lazy right fold
instance AsRM M where
  asR (M k h m z) = R k (m . h) z
  asM = id

-- | We can convert from a lazy left folding to a right or monoidal fold
instance AsRM L where
  asM (L k h z) = M (\f -> k (f z)) (flip h) (>>>) id
  asR (L k h z) = R (\f -> k (f z)) (\b g x -> g (h x b)) id

-- | We can convert from a strict left folding to a right or monoidal fold
instance AsRM L' where
  asR (L' k h z) = R (\f -> k (f z)) (\b g x -> g $! h x b) id

class AsL' p where
  -- | 'asL'' is a folding homomorphism to a strict left folding
  --
  -- @
  -- 'run' xs ('asL'' φ)         ≡ 'run' xs φ
  -- 'prefix' xs ('asL'' φ)      ≡ 'asL'' ('prefix' xs φ)
  -- 'prefixOf' l xs ('asL'' φ)  ≡ 'asL'' ('prefixOf' l xs φ)
  -- 'postfix' ('asL'' φ) xs     ≡ 'asL'' ('postfix' φ xs)
  -- 'postfixOf' l ('asL'' φ) xs ≡ 'asL'' ('postfixOf' l φ xs)
  -- 'left'' ('asL'' φ)          ≡ 'asL'' ('left'' φ)
  -- 'right'' ('asL'' φ)         ≡ 'asL'' ('right'' φ)
  -- 'dimap' l r ('asL'' φ)      ≡ 'asL'' ('dimap' l r φ)
  -- 'extract' ('asL'' φ)        ≡ 'extract' φ
  -- 'pure' a                   ≡ 'asL'' ('pure' a)
  -- 'asL'' φ '<*>' 'asL'' ψ       ≡ 'asL'' (φ '<*>' ψ)
  -- 'return' a                 ≡ 'asL'' ('return' a)
  -- 'asL'' φ '>>=' 'asL'' . k     ≡ 'asL'' (φ '>>=' k)
  -- @
  asL' :: p a b -> L' a b

-- | We can convert a lazy fold to itself
instance AsL' L' where
  asL' = id

-- | We can convert from a lazy left folding to a strict left folding.
instance AsL' L where
  asL' (L k h z) = L' (\(Box r) -> k r) (\(Box r) a -> Box (h r a)) (Box z)

data Box a = Box a
