{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module Data.Fold.Class
  ( Folding(..)
  , beneath
  ) where

import Control.Lens
import Control.Lens.Internal.Setter
import Data.Foldable
import Data.Profunctor.Unsafe

--
-- $setup
-- >>> import Data.Fold

newtype One a = One a

instance Foldable One where
  foldMap f (One a) = f a

class Choice p => Folding p where
  prefix :: Foldable t => t a -> p a b -> p a b
  prefix = prefixOf folded
  {-# INLINE prefix #-}

  prefix1 :: a -> p a b -> p a b
  prefix1 = prefix . One
  {-# INLINE prefix1 #-}

  prefixOf :: Fold s a -> s -> p a b -> p a b

  postfix :: Foldable t => p a b -> t a -> p a b
  postfix = postfixOf folded
  {-# INLINE postfix #-}

  postfix1 :: p a b -> a -> p a b
  postfix1 p = postfix p . One
  {-# INLINE postfix1 #-}

  postfixOf :: Fold s a -> p a b -> s -> p a b

  run :: Foldable t => t a -> p a b -> b
  run = runOf folded
  {-# INLINE run #-}

  run1 :: a -> p a b -> b
  run1 = run . One
  {-# INLINE run1 #-}

  runOf :: Fold s a -> s -> p a b -> b

-- enscanOf :: Traversal s t a b -> s -> p a b -> t

-- | Lift a 'Folding' into a 'Prism'.
--
-- This acts like a generalized notion of \"costrength\",
-- when applied to a 'Folding', causing it to return the
-- left-most value that fails to match the Prism, or the
-- result of accumulating rewrapped in the 'Prism' if
-- everything matches.
--
-- >>> run [Left 1, Left 2, Left 3] $ beneath _Left $ R id (+) 0
-- Left 6
--
-- >>> run [Left 1, Right 2, Right 3] $ beneath _Left $ R id (+) 0
-- Right 2
--
-- @
-- beneath :: Prism s t a b -> p a b -> p s t
-- beneath :: Iso s t a b   -> p a b -> p s t
-- @
beneath :: Profunctor p => Overloaded p Mutator s t a b -> p a b -> p s t
beneath l f = runMutator #. l (Mutator #. f)
