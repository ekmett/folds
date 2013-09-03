{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module Data.Fold.Class
  ( Folding(..)
  , within
  ) where

import Control.Lens
import Control.Lens.Internal.Setter
import Data.Foldable
import Data.Profunctor.Unsafe

--
-- $setup
-- >>> import Data.Fold

class Choice p => Folding p where
  enfold :: Foldable t => t a -> p a b -> b
  enfold = enfoldOf folded

  enfoldOf :: Fold s a -> s -> p a b -> b

  enfold' :: Foldable t => t a -> p a b -> b
  enfold' = enfoldOf' folded

  enfoldOf' :: Fold s a -> s -> p a b -> b
  enfoldOf' = enfoldOf

  enscan :: Traversable t => t a -> p a b -> t b
  enscan = enscanOf traverse

  enscanOf :: Traversal s t a b -> s -> p a b -> t

-- | Lift a 'Folding' into a 'Prism'.
--
-- This acts like a generalized notion of \"costrength\",
-- when applied to a 'Folding', causing it to return the
-- left-most value that fails to match the Prism, or the
-- result of accumulating rewrapped in the 'Prism' if
-- everything matches.
--
-- >>> enfold [Left 1, Left 2, Left 3] $ within _Left $ R id (+) 0
-- Left 6
--
-- >>> enfold [Left 1, Right 2, Right 3] $ within _Left $ R id (+) 0
-- Right 2
--
-- @
-- within :: Prism s t a b -> p a b -> p s t
-- within :: Iso s t a b   -> p a b -> p s t
-- @
within :: Profunctor p => Overloaded p Mutator s t a b -> p a b -> p s t
within l f = runMutator #. l (Mutator #. f)
