{-# LANGUAGE RankNTypes #-}
module Data.Fold.Class
  ( Folding(..)
  ) where

import Control.Lens
import Data.Foldable

class Choice p => Folding p where
  enfold :: Foldable t => t b -> p b a -> a
  enfold = enfoldOf folded

  enfoldOf :: Fold s b -> s -> p b a -> a

  enfold' :: Foldable t => t b -> p b a -> a
  enfold' = enfoldOf' folded

  enfoldOf' :: Fold s b -> s -> p b a -> a
  enfoldOf' = enfoldOf
