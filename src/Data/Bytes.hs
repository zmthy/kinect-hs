-- | Contains a few functions for converting to and from lists of bytes.
module Data.Bytes
    ( -- * Aliases
      Bytes
    
      -- * Integral to Bytes
    , toBigEndianBytes
    , toLittleEndianBytes
    
      -- * Bytes to Integral
    , fromBigEndianBytes
    , fromLittleEndianBytes
    ) where

import Data.Bits(Bits, bitSize, shiftL, shiftR, (.&.), (.|.))
import Data.Word(Word8)

type Bytes = [Word8]

-- | Converts a non-arbitrarily large number to a list of bytes, in ascending
-- order so that the biggest byte is at the end.
toBigEndianBytes :: (Integral a, Bits a) => a -> Bytes

toBigEndianBytes bits = toBigEndianBytes' bits 0 (size bits)

toBigEndianBytes' :: (Integral a, Bits a) => a -> Int -> Int -> Bytes

toBigEndianBytes' bits i n
    | i == n = [smallify bits n]
    | otherwise =  smallify bits i : toBigEndianBytes' bits (i + 1) n

-- | Converts a non-arbitrarily large number to a list of bytes, in
-- descending order so that the smallest byte is at the end.
toLittleEndianBytes :: (Integral a, Bits a) => a -> Bytes

toLittleEndianBytes bits = toLittleEndianBytes' bits (size bits)

toLittleEndianBytes' :: (Integral a, Bits a) => a -> Int -> Bytes

toLittleEndianBytes' bits 0 = [smallify bits 0]
toLittleEndianBytes' bits i =
    smallify bits i : toLittleEndianBytes' bits (i - 1)

-- | Converts a list of bytes in big endian format to a single integral.
fromBigEndianBytes :: (Integral a, Bits a) => Bytes -> a

fromBigEndianBytes bytes = fromBigEndianBytes' bytes (length bytes - 1)

fromBigEndianBytes' :: (Integral a, Bits a) => Bytes -> Int -> a

fromBigEndianBytes' [] _ = 0
fromBigEndianBytes' (byte : bytes) n =
    biggify byte (n - length bytes) .|. fromBigEndianBytes' bytes n

-- | Converts a list of bytes in little endian format to a single integral.
fromLittleEndianBytes :: (Integral a, Bits a) => Bytes -> a

fromLittleEndianBytes [] = 0
fromLittleEndianBytes (byte : bytes) =
    biggify byte (length bytes) .|. fromLittleEndianBytes bytes

size :: (Integral a, Bits a) => a -> Int

size bits = truncate (fromIntegral (bitSize bits) / (8 :: Double)) - 1

smallify :: Bits a => Integral a => a -> Int -> Word8

smallify bits shift = fromIntegral ((shiftR bits (shift * 8)) .&. 255)

biggify :: Integral a => Bits b => a -> Int -> b

biggify byte shift = shiftL (fromIntegral byte) (shift * 8)
