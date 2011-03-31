-- | This example takes the first frame of depth data from the Kinect and
-- converts it into a 3D model in PLY format.
module Main (main) where

import Codec.Model.Ply
import Control.Concurrent
import Control.Exception (finally)
import Data.Word (Word16)
import Device.Kinect
import System.Environment (getArgs)

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
    list <- newEmptyMVar
    done <- newEmptyMVar
    onDepth device (depth list)
    _ <- forkOS (thread context device list done)
    args <- getArgs
    takeMVar done
    takeMVar list >>= save args

depth :: MVar [Word16] -> DataCallback Word16
depth list _ dd _ = putMVar list dd

thread :: Context -> Device -> MVar [Word16] -> MVar () -> IO ()
thread context device list done =
    finally (start >> finally loop stop) (putMVar done ())
  where
    start = startDepth device
    stop  = stopDepth device
    loop  = processEvents context >> isEmptyMVar list >>= empty
    empty True  = loop
    empty False = return ()

save :: [String] -> [Word16] -> IO ()
save args list = savePlyFile "depth.ply" header every
  where
    arg = if length args > 0 then read (head args) else 8
    av  = if arg == 1 || arg == 2 || arg == 4 || arg == 8 then arg else 8
    
    vertices = mapVertices
        (cut av fromIntegral [0..frameWidth - 1])
        (reverse (cut av fromIntegral [0..frameHeight - 1]))
        (flatten (map (cut av fromIntegral)
            (cut av id (split frameWidth (take pixelCount list)))))
    
    every  = clean vertices ++ mapFaces vertices
    header = mkHeader Ascii [] every

mapVertices :: [Float] -> [Float] -> [Float] -> [[Data]]
mapVertices xlist (y : yrest) zlist = mapVertices' xlist y zlist :
    mapVertices xlist yrest (drop (length xlist) zlist)
mapVertices _ _ _ = []

mapVertices' :: [Float] -> Float -> [Float] -> [Data]
mapVertices' (x : xrest) y (z : zrest) =
    Vertex (x, y, 2047 - z) : mapVertices' xrest y zrest
mapVertices' _ _ _ = []

mapFaces :: [[Data]] -> [Data]
mapFaces (one : two : rest) = mapFaces' one two ++ mapFaces (two : rest)
mapFaces _ = []

mapFaces' :: [Data] -> [Data] -> [Data]
mapFaces' (Vertex one@(_, _, z1) : Vertex two@(_, _, z2) : rest1)
        (Vertex three@(_, _, z3) : Vertex four@(_, _, z4) : rest2) =
    (if z1 /= 0 && z2 /= 0 && z3 /= 0 && diff 50 z1 z2 z3
        then (Triangle one two three :) else id) $
    (if z3 /= 0 && z2 /= 0 && z4 /= 0 && diff 50 z3 z2 z4
        then (Triangle three two four :) else id) $
    mapFaces' (Vertex two : rest1) (Vertex four : rest2)
mapFaces' _ _ = []

cut :: Int -> (a -> b) -> [a] -> [b]
cut _ _ [] = []
cut each fn list = fn (head list) : cut each fn (drop each list)

split :: Int -> [a] -> [[a]]
split _ [] = []
split by list = take by list : split by (drop by list)

clean :: [[Data]] -> [Data]
clean [] = []
clean (list : rest) = filter getZ list ++ clean rest

getZ :: Data -> Bool
getZ (Vertex (_, _, z)) = z /= 0
getZ _ = False

flatten :: [[a]] -> [a]
flatten [] = []
flatten (list : rest) = list ++ flatten rest

diff :: Float -> Float -> Float -> Float -> Bool
diff by one two three = one <= two + by && one <= three + by && two <=
    one + by && two <= three + by && three <= one + by && three <= two + by
