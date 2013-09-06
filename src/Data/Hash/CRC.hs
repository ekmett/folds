module Data.Hash.CRC
  (
  -- * 8 bit CRCs
    crc8
  , crc8_ccitt
  , crc8_mvb
  , crc8_sae
  , crc8_wcdma
  -- * 16 bit CRCs
  , crc16
  , crc16_ccitt
  -- * 32 bit CRCs
  , crc32
  , crc32_c
  , crc32_k
  , crc32_q
  -- * 64 bit CRCs
  , crc64_ecma
  , crc64_iso
  ) where

import Data.Bits
import Data.Fold.L'
import Data.Vector.Unboxed as Unboxed
import Data.Word

-- * 64 bit CRCs

-- | Used in <http://en.wikipedia.org/wiki/High-Level_Data_Link_Control HDLC>, <http://en.wikipedia.org/wiki/Swiss-Prot#UniProtKB.2FSwiss-Prot Swiss-Prot> and <http://en.wikipedia.org/wiki/TrEMBL TrEMBL>, but it is <http://www0.cs.ucl.ac.uk/staff/d.jones/crcnote.pdf considered weak>
crc64_iso :: L' Word8 Word64
crc64_iso  = crc crc64_iso_lut
{-# INLINE crc64_iso #-}

-- | Used in <http://www.ecma-international.org/publications/standards/Ecma-182.htm ECMA-182> and <http://en.wikipedia.org/wiki/XZ_Utils XZ Utils>.
crc64_ecma :: L' Word8 Word64
crc64_ecma = crc crc64_ecma_lut
{-# INLINE crc64_ecma #-}

crc64_iso_lut, crc64_ecma_lut :: Unboxed.Vector Word64
crc64_iso_lut  = lut 0xD800000000000000
crc64_ecma_lut = lut 0xc96c5795d7870f42
{-# NOINLINE crc64_iso_lut #-}
{-# NOINLINE crc64_ecma_lut #-}

-- * 32 bit CRCs

-- | Standard CRC-32 as used in HDLC, ANSI X3.66, ITU-T V.42, Ethernet, Serial ATA,
-- MPEG-2, PKZIP, Gzip, Bzip2, PNG, and many other protocols and file formats.
crc32 :: L' Word8 Word32
crc32 = crc crc32_lut
{-# INLINE crc32 #-}

-- | CRC-32 (Castagnoli) as used in iSCSI, SCTP, G.hn payload, SSE4.2, Btrfs, ext4
crc32_c :: L' Word8 Word32
crc32_c = crc crc32_c_lut
{-# INLINE crc32_c #-}

-- | CRC-32 (Koopman)
crc32_k :: L' Word8 Word32
crc32_k = crc crc32_k_lut
{-# INLINE crc32_k #-}

-- | CRC-32 as used in aviation; AIXM
crc32_q :: L' Word8 Word32
crc32_q = crc crc32_q_lut
{-# INLINE crc32_q #-}

crc32_lut, crc32_c_lut, crc32_k_lut, crc32_q_lut :: Unboxed.Vector Word32
crc32_lut  = lut 0xedb88320
crc32_c_lut = lut 0x82f63b78
crc32_k_lut = lut 0xeb31d82e
crc32_q_lut = lut 0xd5828281
{-# NOINLINE crc32_lut #-}
{-# NOINLINE crc32_c_lut #-}
{-# NOINLINE crc32_k_lut #-}
{-# NOINLINE crc32_q_lut #-}

-- * 16 bit CRCs

-- | Used by Bisync, Modbus, USB, ANSI X3.28, SIA DC-07, many others; also known as CRC-16 and CRC-16-ANSI
crc16 :: L' Word8 Word16
crc16 = crc crc16_lut
{-# INLINE crc16 #-}

-- | Used by X.25, V.41, HDLC FCS, XMODEM, Bluetooth, PACTOR, SD, many others; known as CRC-CCITT
crc16_ccitt :: L' Word8 Word16
crc16_ccitt = crc crc16_ccitt_lut
{-# INLINE crc16_ccitt #-}

crc16_lut, crc16_ccitt_lut :: Unboxed.Vector Word16
crc16_lut = lut 0xA001
crc16_ccitt_lut = lut 0x8408
{-# NOINLINE crc16_lut #-}
{-# NOINLINE crc16_ccitt_lut #-}

-- * 8 bit CRCs

crc8 :: L' Word8 Word8
crc8 = crc crc8_lut
{-# INLINE crc8 #-}

-- | Used by <http://www.itu.int/rec/T-REC-I.432.1-199902-I/en I.432.1>; <http://en.wikipedia.org/wiki/Asynchronous_Transfer_Mode ATM> and <http://en.wikipedia.org/wiki/ISDN ISDN> <http://en.wikipedia.org/wiki/CRC-based_framing HEC> and cell delineation
crc8_ccitt :: L' Word8 Word8
crc8_ccitt = crc crc8_ccitt_lut
{-# INLINE crc8_ccitt #-}

-- | Used by <http://en.wikipedia.org/wiki/Train_Communication_Network Train Communication Network>, <http://en.wikipedia.org/wiki/IEC_60870-5 IEC 60870-5>
crc8_mvb :: L' Word8 Word8
crc8_mvb = crc crc8_mvb_lut
{-# INLINE crc8_mvb #-}

-- | Used by <http://en.wikipedia.org/wiki/AES3 AES3>
crc8_sae :: L' Word8 Word8
crc8_sae = crc crc8_sae_lut
{-# INLINE crc8_sae #-}

-- | Used by <http://books.google.co.uk/books?id=yN5lve5L4vwC&lpg=PA223&dq=&pg=PA223#v=onepage&q&f=false WCDMA>
crc8_wcdma :: L' Word8 Word8
crc8_wcdma = crc crc8_wcdma_lut
{-# INLINE crc8_wcdma #-}

crc8_lut, crc8_ccitt_lut, crc8_mvb_lut, crc8_sae_lut, crc8_wcdma_lut :: Unboxed.Vector Word8
crc8_lut       = lut 0xab
crc8_ccitt_lut = lut 0xe0
crc8_mvb_lut   = lut 0x53
crc8_sae_lut   = lut 0xb8
crc8_wcdma_lut = lut 0xd9
{-# NOINLINE crc8_lut #-}
{-# NOINLINE crc8_ccitt_lut #-}
{-# NOINLINE crc8_mvb_lut #-}
{-# NOINLINE crc8_sae_lut #-}
{-# NOINLINE crc8_wcdma_lut #-}

-- * Implementation

crc :: (Integral a, Bits a, Unbox a) => Unboxed.Vector a -> L' Word8 a
crc l = L' complement step (-1) where
  step r b = unsafeShiftR r 8 `xor` l Unboxed.! fromIntegral (xor r (fromIntegral b) .&. 0xff)
{-# INLINE crc #-}

lut :: (Num a, Bits a, Unbox a) => a -> Unboxed.Vector a
lut seed = Unboxed.generate 256 (go.go.go.go.go.go.go.go.fromIntegral) where
  go c = unsafeShiftR c 1 `xor` if c .&. 1 /= 0 then seed else 0
