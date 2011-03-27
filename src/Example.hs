module Main (main) where

import Codec.Image.Bmp (Rgb, saveBmpFile)
import Control.Concurrent
import Control.Exception (finally)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word8, Word16)
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
    setTiltAngle device 30
    setDepthFormat device D11Bit
    done  <- newEmptyMVar
    end   <- newEmptyMVar
    reply <- newEmptyMVar
    onDepth device (depth done)
    _ <- forkOS (thread context device end reply)
    takeMVar done
    putMVar end ()
    takeMVar reply

depth :: MVar () -> DataCallback Word16
depth end _ dd _ =
    finally (saveBmpFile "/Users/Tim/Desktop/depth.bmp" (640, 480) rgb)
            (putMVar end ())
  where
    rgb = depthToRgb dd 0

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

thread :: Context -> Device -> MVar () -> MVar () -> IO ()
thread context device end reply =
    finally (start >> finally loop stop) (putMVar reply ())
  where
    start = startDepth device
    stop  = stopDepth device
    loop  = processEvents context >> isEmptyMVar end >>= empty
    empty True  = loop
    empty False = return ()
