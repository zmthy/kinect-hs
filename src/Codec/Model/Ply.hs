module Codec.Mesh.Ply
    ( -- * Data Types 
      Header
    , Vertex
    , Colour
    , Data(..)
    , Format(..)
    
      -- * Helpers
    , mkHeader
    , showFile
    
      -- * Saving
    , savePlyFile
    ) where

import Control.Exception (PatternMatchFail(..), throw)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as Ascii
import Data.Bytes (Bytes, toBigEndianBytes, toLittleEndianBytes)
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Word (Word, Word8)

data Header = Header Format [Info]
            deriving (Eq, Show)

data Format = Ascii
            | Binary Endian
            deriving (Eq, Show)

data Endian = Little
            | Big
            deriving (Eq, Show)

data Info = Comment String
          | Element String Word [Property]
          deriving (Eq, Show)

data Property = Property Type String
              deriving (Eq, Show)

data Type = Char
          | UChar
          | Short
          | UShort
          | Int
          | UInt
          | Float
          | Double
          | List Type Type
          deriving (Eq, Show)

type Vertex = (Int, Int, Int)

type Colour = (Word8, Word8, Word8)

data Data = Vertex Vertex
          | Triangle Vertex Vertex Vertex
          | Square Vertex Vertex Vertex Vertex
          | Face [Int]
          | Edge Vertex Vertex
          deriving (Eq, Show)

-- | Generates a header from the given data.
mkHeader :: Format -> [String] -> [Data] -> Header
mkHeader format comments dat = Header format (map Comment comments ++
    catMaybes [vertex (vertices dat), face (faces dat), edge (edges dat)])

vertex :: Word -> Maybe Info
vertex 0 = Nothing
vertex i = Just $ Element "vertex" i
    [ Property Float "x"
    , Property Float "y"
    , Property Float "z" ]

face :: Word -> Maybe Info
face 0 = Nothing
face i = Just $ Element "face" i [ Property (List UChar Int) "vertex_index" ]

edge :: Word -> Maybe Info
edge 0 = Nothing
edge i = Just $ Element "edge" i
    [ Property Int "vertex1"
    , Property Int "vertex2" ]

vertices :: [Data] -> Word
vertices = vertices' 0

vertices' :: Word -> [Data] -> Word
vertices' i ((Vertex _) : rest) = vertices' (i + 1) rest
vertices' i (_ : rest) = vertices' i rest
vertices' i [] = i

faces :: [Data] -> Word
faces = faces' 0

faces' :: Word -> [Data] -> Word
faces' i ((Triangle _ _ _) : rest) = faces' (i + 1) rest
faces' i ((Square _ _ _ _) : rest) = faces' (i + 1) rest
faces' i (_ : rest) = faces' i rest
faces' i [] = i

edges :: [Data] -> Word
edges = edges' 0

edges' :: Word -> [Data] -> Word
edges' i ((Edge _ _) : rest) = edges' (i + 1) rest
edges' i (_ : rest) = edges' i rest
edges' i [] = i

savePlyFile :: FilePath -> Header -> [Data] -> IO ()
savePlyFile path header list = writeFile path (showFile header list)
    -- | otherwise = ByteString.writeFile path (packFile header list)

showFile :: Header -> [Data] -> String
showFile header list = showHead header ++ showData list

showHead :: Header -> String
showHead (Header form info) = "ply\nformat " ++ showFormat form ++
    " 1.0\n" ++ showHead' info ++ "end_header" ++ "\n"

showFormat :: Format -> String
showFormat Ascii           = "ascii"
showFormat (Binary Little) = "binary_little_endian"
showFormat (Binary Big)    = "binary_big_endian"

showHead' :: [Info] -> String
showHead' ((Comment comment) : rest) =
    "comment " ++ comment ++ "\n" ++ showHead' rest
showHead' ((Element name count props) : rest) =
    "element " ++ name ++ " " ++ show count ++ "\n" ++
    showProperties props ++ showHead' rest
showHead' [] = ""

showProperties :: [Property] -> String
showProperties ((Property typ name) : rest) =
    "property " ++ showType typ ++ " " ++ name ++ "\n" ++
    showProperties rest
showProperties [] = ""

showType :: Type -> String
showType Char   = "char"
showType UChar  = "uchar"
showType Short  = "short"
showType UShort = "ushort"
showType Int    = "int"
showType UInt   = "uint"
showType Float  = "float"
showType Double = "double"
showType (List count typ) = "list " ++ showType count ++ " " ++ showType typ

showData :: [Data] -> String
showData list = let v = nub list in
    init (unlines (catMaybes (showData' v v)))

showData' :: [Data] -> [Data] -> [Maybe String]
showData' (next : rest) orig = showData'' orig next : showData' rest orig
showData' [] _ = []

showData'' :: [Data] -> Data -> Maybe String
showData'' _ (Vertex (x, y, z)) = Just (unwords (map show [x, y, z]))
showData'' o (Triangle v1 v2 v3) = tryWords (map (find o) [v1, v2, v3])
showData'' o (Square v1 v2 v3 v4) = tryWords (map (find o) [v1, v2, v3, v4])
showData'' o (Edge v1 v2) = tryWords (map (find o) [v1, v2])

tryWords :: [Maybe Word] -> Maybe String
tryWords list = if elem Nothing list then Nothing
    else Just (unwords (map show (fromIntegral
        (length list) : catMaybes list)))

find :: [Data] -> Vertex -> Maybe Word
find list v = find' (getVertices list) v 0

find' :: [Vertex] -> Vertex -> Word -> Maybe Word
find' (next : rest) v i | next == v = Just i
                        | otherwise = find' rest v (i + 1)
find' [] _ _ = Nothing

getVertices :: [Data] -> [Vertex]
getVertices ((Vertex v) : rest) = v : getVertices rest
getVertices (_ : rest) = getVertices rest
getVertices [] = []

-- packFile :: Header -> [Data] -> ByteString
-- packFile header list =
--     ByteString.concat [packHead header, packData header list]
-- 
-- packHead :: Header -> ByteString
-- packHead header = Ascii.pack (showHead header)
-- 
-- packData :: Header -> [Data] -> ByteString
-- packData header list = ByteString.pack ((case endian header of
--     Little -> concat . map toLittleEndianBytes
--     Big -> concat . map toBigEndianBytes) (packData' list))
-- 
-- packData' :: [Data] -> Bytes
-- packData' (next : rest) = convertData next ++ packData' rest
-- packData' [] = []
-- 
-- convertData :: [a] -> Bytes
-- convertData (next : rest) = convertData' (show next) : convertData rest
-- convertData [] = []
-- 
-- convertData' :: String -> Word8
-- convertData' value = if length value == 1 && digitToInt (head value) > 9
--     then ByteString.head (Ascii.pack value)
--     else read value

format :: Header -> Format
format (Header f _) = f

endian :: Header -> Endian
endian (Header (Binary e) _) = e
endian _ = throw (PatternMatchFail "ASCII format passed to endian test")
