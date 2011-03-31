-- | This example takes the first frame of depth data from the Kinect and
-- converts it into an RGB heatmap BMP image.
module Main (main) where

import Codec.Image.Bmp (Rgb, saveBmpFile)
import Control.Concurrent
import Control.Exception (finally)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word16)
import Device.Kinect

main :: IO ()
main = do
    context <- initialize
    finally (deviceCount context >>= gate context) (shutdown context)
  where
    gate _ 0 = ioError (userError "No devices found")
    gate context _ = do
        device <- openDevice context 0
        finally (begin context device) (closeDevice device)

begin :: Context -> Device -> IO ()
begin context device = do
    setLed device Green
    setTiltAngle device 20
    setDepthFormat device D11Bit
    list <- newEmptyMVar
    done <- newEmptyMVar
    onDepth device (depth list)
    _ <- forkOS (thread context device list done)
    takeMVar done
    takeMVar list >>= save

depth :: MVar [Word16] -> DataCallback Word16
depth list _ dd _ = putMVar list (take pixelCount dd)

thread :: Context -> Device -> MVar [Word16] -> MVar () -> IO ()
thread context device list done =
    finally (start >> finally loop stop) (putMVar done ())
  where
    start = startDepth device
    stop  = stopDepth device
    loop  = processEvents context >> isEmptyMVar list >>= empty
    empty True  = loop
    empty False = return ()

save :: [Word16] -> IO ()
save list = saveBmpFile "depth.bmp" (640, 480) rgb
  where
    rgb = depthToRgb list 0

depthToRgb :: [Word16] -> Int -> Rgb
depthToRgb (d : dd) i
    | i == pixelCount = []
    | otherwise = depthCase (gamma d) ++ depthToRgb dd (i + 1)
depthToRgb [] _ = []

depthCase :: Word16 -> Rgb
depthCase pval = case shiftR pval 8 of
    0 -> [255, 255 - lb, 255 - lb]
    1 -> [255, lb, 0]
    2 -> [255 - lb, 255, 0]
    3 -> [0, 255, lb]
    4 -> [0, 255 - lb, 255]
    5 -> [0, 0, 255 - lb]
    _ -> [0, 0, 0]
  where
    lb = fromIntegral (pval .&. 0xff)

gamma :: Word16 -> Word16
gamma i = truncate $
    (((fromIntegral i / 2048.0) ^ (3 :: Int) * 9216) :: Double)
