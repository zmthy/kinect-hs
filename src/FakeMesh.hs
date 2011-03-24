module Main where

import Codec.Mesh.Ply
import Data.IORef
import Data.List (elemIndex)
import System.IO.Unsafe (unsafePerformIO)

type PlyMap = (Int, Int, Int) -> [Data]

width :: Int
width = 640

height :: Int
height = 480

main :: IO ()
main = getList >>= depth
    
getList :: IO [Int]
getList = readFile "/Users/Tim/Documents/Internship/Kinect/kinect.txt" >>=
    return . parseList

parseList :: String -> [Int]
parseList ('[' : rest) = parseList rest
parseList str = case elemIndex ',' str of
    Just 0 -> parseList (tail str)
    Just i -> read (take i str) : parseList (drop (i + 1) str)
    Nothing -> []

depth :: [Int] -> IO ()
depth list = print (length ply)
    -- >> putStrLn (take 50 (showFile (mkHeader Ascii [] ply) ply))
  where
    lists = take height (split list width)
    ply   = values lists 0

split :: [a] -> Int -> [[a]]
split list len = take len list : split (drop len list) len

values :: [[Int]] -> Int -> [Data]
values [next] pos = vertices next (0, pos)
values (n1 : n2 : rest) pos = sort $ vertices n1 (0, pos) ++
    faces (n1, n2) (0, pos) ++ values (n2 : rest) (pos + 1)
values _ _ = []

vertices :: [Int] -> (Int, Int) -> [Data]
vertices (next : rest) (x, y) =
    Vertex (x, y, next) : vertices rest (x + 1, y)
vertices _ _ = []

faces :: ([Int], [Int]) -> (Int, Int) -> [Data]
faces ([_], [_]) _ = []
faces ((v1 : v2 : r1), (v3 : v4 : r2)) (x, y) =
    [ Triangle (x, y, v1) (x + 1, y, v2) (x, y + 1, v3)
    , Triangle (x + 1, y, v2) (x + 1, y + 1, v4) (x, y + 1, v3) ] ++
        faces (v2 : r1, v4 : r2) (x + 1, y)
faces _ _ = []

sort :: [Data] -> [Data]
sort list = onlyVertex list ++ onlyFace list

onlyVertex :: [Data] -> [Data]
onlyVertex (v@(Vertex x) : rest) = v : onlyVertex rest
onlyVertex (_ : rest) = onlyVertex rest
onlyVertex [] = []

onlyFace :: [Data] -> [Data]
onlyFace (f@(Triangle _ _ _) : rest) = f : onlyFace rest
onlyFace (_ : rest) = onlyFace rest
onlyFace [] = []
