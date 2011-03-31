-- | This example takes the first frame of video data from the Kinect and saves
-- it as an RGB BMP image.
module Main (main) where

import Codec.Image.Bmp (saveBmpFile)
import Control.Concurrent
import Control.Exception (finally)
import Data.Word (Word8)
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
    setVideoFormat device RGB
    list <- newEmptyMVar
    done <- newEmptyMVar
    onVideo device (video list)
    _ <- forkOS (thread context device list done)
    takeMVar done
    takeMVar list >>= save

video :: MVar [Word8] -> DataCallback Word8
video list _ rgb _ = putMVar list (take pixelCount rgb)

thread :: Context -> Device -> MVar [Word8] -> MVar () -> IO ()
thread context device list done =
    finally (start >> finally loop stop) (putMVar done ())
  where
    start = startVideo device
    stop  = stopVideo device
    loop  = processEvents context >> isEmptyMVar list >>= empty
    empty True  = loop
    empty False = return ()

save :: [Word8] -> IO ()
save rgb = saveBmpFile "image.bmp" (640, 480) rgb
