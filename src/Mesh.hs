module Main where

import Codec.Mesh.Ply
import Control.Concurrent
import Control.Exception(finally)
import Data.Word(Word16)
import Device.Kinect

type PlyMap = (Int, Int, Int) -> [Data]

width :: Int
width = frameWidth

height :: Int
height = frameHeight

main :: IO ()
main = do
    context <- setup
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

(//) :: Integral a => a -> a -> a
a // b = truncate $ fromIntegral a / ((fromIntegral b) :: Double)
infixl 7 //

depth :: MVar () -> DataCallback Word16
depth end _ list _ = finally
    (savePlyFile "/Users/Tim/Desktop/kinect.ply" (mkHeader Ascii [] ply) ply)
    (putMVar end ())
  where
    vertices = mapVertices list
    ply = vertices -- ++ mapFaces vertices list

mapVertices :: [Word16] -> [Data]
mapVertices list = traverse list (0, 0) 0 ((: []) . Vertex)

mapFaces :: [Data] -> [Word16] -> [Data]
mapFaces vs list = traverse list (0, 0) 1 $ \v -> case getVertices v vs of
    Just [v1, v2, v3, v4] -> [ Triangle v1 v2 v4 , Triangle v2 v3 v4 ]
    _ -> []

getVertices :: Vertex -> [Data] -> Maybe [Vertex]
getVertices (x, y, _) list = do
    v1 <- getVertex (x, y) list
    v2 <- getVertex (x + 1, y) list
    v3 <- getVertex (x + 1, y + 1) list
    v4 <- getVertex (x, y + 1) list
    return [v1, v2, v3, v4]

getVertex :: (Int, Int) -> [Data] -> Maybe Vertex
getVertex (x1, y1) ((Vertex (x, y, z)) : rest)
    | x == x1 && y == y1 = Just (x, y, z)
    | otherwise = getVertex (x1, y1) rest
getVertex v (_ : rest) = getVertex v rest
getVertex _ [] = Nothing

traverse :: [Word16] -> (Int, Int) -> Int -> PlyMap -> [Data]
traverse list (x, y) disp ply
    | y >= height - disp = []
    | x >= width - disp =
        traverse (drop disp list) (0, y + 1) disp ply
    | otherwise = ply (x, y, (fromIntegral (head list))) ++
        traverse (tail list) (x + 1, y) disp ply
    --     traverse (drop ((per - 1) * frameWidth) list) (0, y + per) disp ply
    -- | otherwise = ply (x, y, (fromIntegral (head list))) ++
    --     traverse (drop per list) (x + per, y) disp ply
    -- | otherwise = ply (x, y, average list) ++
    --     traverse (drop per list) (x + per, y) disp ply

-- average :: [Word16] -> Int
-- average list = truncate $ fromIntegral (average' list 0) /
--     (fromIntegral (per ^ (2 :: Int)) :: Double)
-- 
-- average' :: [Word16] -> Int -> Word16
-- average' list count
--     | count == per = 0
--     | otherwise = sum (take per (drop (count * frameWidth // per) list)) +
--     average' list (count + 1)

thread :: Context -> Device -> MVar () -> MVar () -> IO ()
thread context device end fin =
    finally (start >> finally loop stop) (putMVar fin ())
  where
    start = startDepth device
    stop  = stopDepth device
    loop  = processEvents context >> isEmptyMVar end >>= empty
    empty True  = loop
    empty False = return ()
