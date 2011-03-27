module Main where

import Codec.Model.Ply
import Control.Concurrent
import Control.Exception (finally)
import Data.Word (Word, Word16)
import Device.Kinect
import System.IO.Unsafe (unsafePerformIO)

type PlyMap = (Int, Int, Int) -> [Data]

main :: IO ()
main = do
    context <- initialize
    finally (deviceCount context >>= gate context) (shutdown context)
  where
    gate _ 0 = putStrLn "No devices found."
    gate context _ = do
        device <- openDevice context 0
        finally (begin context device) (closeDevice device)

begin :: Context -> Device -> IO ()
begin context device = do
    setLed device Green
    setTiltAngle device 20
    setDepthFormat device D11Bit
    end <- newEmptyMVar
    fin <- newEmptyMVar
    onDepth device (depth end)
    _ <- forkOS (thread context device end fin)
    takeMVar fin

average :: Int
average = 2

xlist :: [Float]
xlist = averageEach [0..fromIntegral (frameWidth - 1)] average

ylist :: [Float]
ylist = reverse (averageEach [0..fromIntegral (frameHeight - 1)] average)

depth :: MVar () -> DataCallback Word16
depth end _ ref _ = finally
    (savePlyFile "/Users/Tim/Desktop/kinect.ply" header vertices)
    (putMVar end ())
  where
    zlist = averageEach (take pixelCount (map fromIntegral ref)) (average ^ 2)
    vertices = mapVertices xlist ylist zlist
    header = mkHeader Ascii [] vertices

mapVertices :: [Float] -> [Float] -> [Float] -> [Data]
mapVertices [] (y : rest) z = mapVertices xlist rest z
mapVertices (x : xrest) y (z : zrest) = Vertex (x, head y, z) :
    mapVertices xrest y zrest
mapVertices _ _ _ = []

averageEach :: [Float] -> Int -> [Float]
averageEach [] _ = []
averageEach list each = sum (take each list) / fromIntegral each :
    averageEach (drop each list) each

thread :: Context -> Device -> MVar () -> MVar () -> IO ()
thread context device end fin =
    finally (start >> finally loop stop) (putMVar fin ())
  where
    start = startDepth device
    stop  = stopDepth device
    loop  = processEvents context >> isEmptyMVar end >>= empty
    empty True  = loop
    empty False = return ()
