{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
module Data.Fold.Class
  ( Scan(..)
  , Folding(..)
  , beneath
  ) where

import Control.Lens
import Data.Foldable
import Data.Fold.Internal
import Data.Profunctor.Unsafe

--
-- $setup
-- >>> import Data.Fold

class Choice p => Scan p where
  prefix1 :: a -> p a b -> p a b
#ifndef HLINT
  default prefix1 :: Folding p => a -> p a b -> p a b
#endif
  prefix1 = prefix . An
  {-# INLINE prefix1 #-}

  postfix1 :: p a b -> a -> p a b
#ifndef HLINT
  default postfix1 :: Folding p => p a b -> a -> p a b
#endif
  postfix1 p = postfix p . An
  {-# INLINE postfix1 #-}

  -- | Apply a 'Folding' to a single element of input
  run1 :: a -> p a b -> b
#ifndef HLINT
  default run1 :: Folding p => a -> p a b -> b
#endif
  run1 = run . An
  {-# INLINE run1 #-}

  interspersing :: a -> p a b -> p a b

class Scan p => Folding p where
  -- | Partially apply a 'Folding' to some initial input on the left.
  --
  prefix :: Foldable t => t a -> p a b -> p a b
  prefix = prefixOf folded
  {-# INLINE prefix #-}

  prefixOf :: Fold s a -> s -> p a b -> p a b

  postfix :: Foldable t => p a b -> t a -> p a b
  postfix = postfixOf folded
  {-# INLINE postfix #-}

  postfixOf :: Fold s a -> p a b -> s -> p a b

  -- | Apply a 'Folding' to a container full of input:
  --
  -- >>> run ["hello","world"] $ L id (++) []
  -- "helloworld"
  --
  -- >>> run [1,2,3] $ L id (+) 0
  -- 6
  run :: Foldable t => t a -> p a b -> b
  run = runOf folded
  {-# INLINE run #-}

  runOf :: Fold s a -> s -> p a b -> b

  filtering :: (a -> Bool) -> p a b -> p a b

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
beneath :: Profunctor p => Optic p Identity s t a b -> p a b -> p s t
beneath l f = runIdentity #. l (Identity #. f)
