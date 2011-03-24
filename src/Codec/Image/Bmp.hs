-- | Exports a single function for saving RGB data as a BMP format image.
module Codec.Image.Bmp
    ( -- * Type Aliases
      Rgb
    , Size
    
      -- * Saving
    , saveBmpFile
    ) where

import Data.ByteString.Lazy(ByteString, pack)
import qualified Data.ByteString.Lazy as ByteString
import Data.Word(Word16, Word32)

import Data.Bytes(Bytes, toBigEndianBytes)

-- | Alias for the RGB data as a list of bytes. Every third value is a pixel,
-- and pixels are arranged by row.
type Rgb = Bytes

-- | A quick mask for a 2-dimensional area, in the format (width, height).
type Size = (Int, Int)

-- | Saves a BMP file with the given size and data to the given file path.
-- Note that the given RGB data must be at least as big as the area defined
-- by the size, and any extra data will be ignored.
saveBmpFile :: FilePath -> Size -> Rgb -> IO ()
saveBmpFile path size pixels =
    ByteString.writeFile path (packBmpFile size (reorder size pixels))

-- | Packs the BMP header and data togther into a 'ByteString'.
packBmpFile :: Size -> Rgb -> ByteString
packBmpFile size pixels = pack
    (bmpFileHeader (bmpSize size) ++ bmpInfoHeader size ++ get size pixels)

-- | Changes top-down RGB data to BMP's bottom-up GBR.
reorder :: Size -> Rgb -> Rgb
reorder size rgb = reorder' size (rgbToGbr rgb) 0

reorder' :: Size -> Rgb -> Int -> Rgb
reorder' size@(width, height) pixels i
    | i == height = []
    | otherwise = take (width * 3) (drop (width * (height - 1 - i) * 3)
        pixels) ++ reorder' size pixels (i + 1)

-- | Reorders every RGB pixel to match BMP's BGR format.
rgbToGbr :: Rgb -> Rgb
rgbToGbr pixels = reverse (take 3 pixels) ++ rgbToGbr (drop 3 pixels)

-- | Extracts the required data from an assumedly as-long or longer list.
get :: Size -> Rgb -> Rgb
get (width, height) pixels = take (width * height * 3) pixels

-- | The byte size of 24 bit coloured BMP file with the given size.
bmpSize :: Size -> Word32
bmpSize (width, height) = fromIntegral (width * height * 3 + 54)

-- | The file header of a BMP file with the given byte size.
bmpFileHeader :: Word32 -> Bytes
bmpFileHeader size = h ++ s ++ [0, 0, 0, 0] ++ o
  where
    h = toBigEndianBytes (19778 :: Word16)
    s = toBigEndianBytes size
    o = toBigEndianBytes (54 :: Word32)

-- | The info header of a BMP file with the given dimensions.
bmpInfoHeader :: Size -> Bytes
bmpInfoHeader (width, height) = s ++ w ++ h ++ p ++ c ++ take 24 (cycle [0])
  where
    s = toBigEndianBytes (40 :: Word32)
    w = toBigEndianBytes ((fromIntegral width) :: Word32)
    h = toBigEndianBytes ((fromIntegral height) :: Word32)
    p = toBigEndianBytes (1 :: Word16)
    c = toBigEndianBytes (24 :: Word16)
