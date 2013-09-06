module Data.Hash.Adler32
  ( adler32
  ) where

import Data.Bits
import Data.Fold.L'
import Data.Word

data Adler32 = Adler32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32

-- | <http://en.wikipedia.org/wiki/Adler-32 Adler-32> is a modified Fletcher checksum
-- used in <http://en.wikipedia.org/wiki/Zlib zlib> designed by
-- <http://en.wikipedia.org/wiki/Mark_Adler Mark Adler>.
adler32 :: L' Word8 Word32
adler32 = L' done step (Adler32 1 0) where
  step (Adler32 s1 s2) x = Adler32 s1' s2' where
    s1' = mod (s1 + fromIntegral x) 65521
    s2' = mod (s1' + s2) 65521
  done (Adler32 s1 s2) = unsafeShiftL s2 16 + s1
{-# INLINE adler32 #-}
