{-# LANGUAGE DefaultSignatures #-}
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
  -- * Scaners and Foldings
    Scan(..)
  , Folding(..)
  -- * Combinators
  , beneath
  -- * Scans
  -- ** Left Scans
  , L1(..)  -- lazy Mealy machine
  , L1'(..) -- strict Mealy machine
  -- ** Semigroup Scans
  , M1(..) -- semigroup reducer
  -- ** Right Scans
  , R1(..) -- reversed lazy Mealy machine
  -- * Foldings
  -- ** Left Foldings
  , L(..) -- lazy Moore machine
  , L'(..) -- strict Moore machine
  -- ** Monoidal Foldings
  , M(..) -- monoidal reducer
  -- ** Right Foldings
  , R(..) -- reversed lazy Moore machine
  -- * Homomorphisms
  -- ** Scan Homomorphisms
  -- $scanhom
  , AsRM1(..)
  , AsL1'(..)

  -- ** Folding Homomorphisms
  -- $foldinghom
  , AsRM(..)
  , AsL'(..)
  ) where

import Data.Fold.Class
import Data.Fold.Internal
import Data.Fold.L
import Data.Fold.L'
import Data.Fold.L1
import Data.Fold.L1'
import Data.Fold.M
import Data.Fold.M1
import Data.Fold.R
import Data.Fold.R1
import Control.Category ((>>>))

-- * Scan Homomorphisms

-- $scanhom
--
-- We define @f@ to be a scan homomorphism between @p@ and @q@ when:
--
-- @
-- f :: forall a b. p a b -> q a b
-- @
--
-- @
-- 'run1' xs (f φ)        ≡ 'run1' xs φ
-- 'prefix1' xs (f φ)     ≡ f ('prefix1' xs φ)
-- 'postfix1' (f φ) xs    ≡ f ('postfix1' φ xs)
-- 'dimap' l r (f φ)      ≡ f ('dimap' l r φ)
-- 'pure' a               ≡ f ('pure' a)
-- f φ '<*>' f ψ          ≡ f (φ '<*>' ψ)
-- 'return' a             ≡ f ('return' a)
-- f φ '>>=' f . k        ≡ f (φ '>>=' k)
-- 'interspersing' a (f φ) ≡ f ('interspersing' a φ)
-- @
--
-- Furthermore,
--
-- @'left'' (f φ)@ and @f ('left'' φ)@ should agree whenever either answer is 'Right'
-- @'right'' (f φ)@ and @f ('right'' φ)@ should agree whenver either answer is 'Left'
--

-- * Folding Homomorphisms

-- $foldinghom
--
-- We define @f@ to be a folding homomorphism between @p@ and @q@ when
-- @f@ is a scan homomorphism and additionally we can satisfy:
--
-- @
-- 'run' xs (f φ)         ≡ 'run' xs φ
-- 'runOf' l xs (f φ)     ≡ 'runOf' l xs φ
-- 'prefix' xs (f φ)      ≡ f ('prefix' xs φ)
-- 'prefixOf' l xs (f φ)  ≡ f ('prefixOf' l xs φ)
-- 'postfix' (f φ) xs     ≡ f ('postfix' φ xs)
-- 'postfixOf' l (f φ) xs ≡ f ('postfixOf' l φ xs)
-- 'extract' (f φ)        ≡ 'extract' φ
-- 'filtering' p (f φ)     ≡ f ('filtering' p φ)
-- @
--
-- Note: A law including 'extend' is explicitly excluded. To work consistenly
-- across foldings, use 'prefix' and 'postfix' instead.

class AsRM1 p where
  -- | 'asM1' is a scan homomorphism to a semigroup reducer
  asM1 :: p a b -> M1 a b
  asM1 = asM1.asR1

  -- | 'asM1' is a scan homomorphism to a right scan
  asR1 :: p a b -> R1 a b
  asR1 = asR1.asM1

instance AsRM1 L where
  asM1 (L k h z) = M1 (\f -> k (f z)) (flip h) (>>>)

instance AsRM1 L' where
  asR1 (L' k h z) = R1 (\f -> k (f z)) (\b g x -> g $! h x b) (\a x -> h x a)

instance AsRM1 L1 where
  asM1 (L1 k h z) = M1 (\(Pair' _ r) -> k r) (\a -> Pair' (`h` a) (z a)) (\(Pair' r2r' r') (Pair' r2r _) -> Pair' (r2r.r2r') (r2r r'))

instance AsRM1 L1' where
  asM1 (L1' k h z) = M1 (\(Pair' _ r) -> k r) (\a -> Pair' (`h` a) (z a)) (\(Pair' r2r' r') (Pair' r2r _) -> Pair' (\r -> r2r $! r2r' r) (r2r r'))

instance AsRM1 M where
  asM1 (M k h m _) = M1 k h m
  asR1 (M k h m _) = R1 k (m.h) h

instance AsRM1 M1 where
  asM1 = id
  asR1 (M1 k h m) = R1 k (m.h) h

instance AsRM1 R where
  asM1 (R k h z) = M1 (\f -> k (f z)) h (.)
  asR1 (R k h z) = R1 k h (\a -> h a z)

instance AsRM1 R1 where
  asM1 (R1 k h z) = M1 (\(Pair' _ r) -> k r) (\a -> Pair' (h a) (z a)) (\(Pair' r2r _) (Pair' r2r' r') -> Pair' (r2r.r2r') (r2r r'))
  asR1 = id

class AsRM1 p => AsRM p where
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
  -- 'filtering' p ('asM' φ)     ≡ 'asM' ('filtering' p φ)
  -- 'interspersing' a ('asM' φ) ≡ 'asM' ('interspersing' a φ)
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
  -- 'filtering' p ('asR' φ)     ≡ 'asR' ('filtering' p φ)
  -- 'interspersing' a ('asR' φ) ≡ 'asR' ('interspersing' a φ)
  -- @
  asR :: p a b -> R a b
  asR = asR . asM

-- | We can convert from a lazy right fold to a monoidal fold
instance AsRM R where
  asM (R k h z) = M (\f -> k (f z)) h (.) id
  asR = id

-- | We can convert from a monoidal fold to a lazy right fold
instance AsRM M where
  asR (M k h m z) = R k (m.h) z
  asM = id

-- | We can convert from a lazy left folding to a right or monoidal fold
instance AsRM L where
  asM (L k h z) = M (\f -> k (f z)) (flip h) (>>>) id
  asR (L k h z) = R (\f -> k (f z)) (\b g x -> g (h x b)) id

-- | We can convert from a strict left folding to a right or monoidal fold
instance AsRM L' where
  asR (L' k h z) = R (\f -> k (f z)) (\b g x -> g $! h x b) id

class AsRM1 p => AsL1' p where
  -- | Scan homomorphism to a strict Mealy machine
  asL1' :: p a b -> L1' a b
  default asL1' :: AsL' p => p a b -> L1' a b
  asL1' = asL1'.asL'

instance AsL1' L1' where
  asL1' = id

instance AsL1' L1 where
  asL1' (L1 k h z) = L1' (\(Box r) -> k r) (\(Box r) a -> Box (h r a)) (\a -> Box (z a))

instance AsL1' L where
  asL1' (L k h z) = L1' (\(Box r) -> k r) (\(Box r) a -> Box (h r a)) (\a -> Box (h z a))

instance AsL1' L' where
  asL1' (L' k h z) = L1' k h (h z)

class (AsRM p, AsL1' p) => AsL' p where
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
  -- 'filtering' p ('asL'' φ)     ≡ 'asL'' ('filtering' p φ)
  -- 'interspersing' a ('asL'' φ) ≡ 'asL'' ('interspersing' a φ)
  -- @
  asL' :: p a b -> L' a b

-- | We can convert a lazy fold to itself
instance AsL' L' where
  asL' = id

-- | We can convert from a lazy left folding to a strict left folding.
instance AsL' L where
  asL' (L k h z) = L' (\(Box r) -> k r) (\(Box r) a -> Box (h r a)) (Box z)
