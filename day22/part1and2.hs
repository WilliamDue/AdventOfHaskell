import qualified Data.Map as M
import Data.List.Split ( splitOn )
import qualified Data.Bifunctor as B
import qualified Data.Set as S
import Data.Char (toUpper)
import Data.Array (range)

data State = On | Off deriving (Show, Read)
data Line = Line { min' :: !Int, max' :: !Int } deriving (Show, Eq)
data Cuboid = Cuboid { x :: !Line, y :: !Line, z :: !Line } deriving (Show, Eq)

listToCuboid :: [[Int]] -> Cuboid
listToCuboid [[xx, xx'], [yy, yy'], [zz, zz']] = Cuboid (Line xx xx') (Line yy yy') (Line zz zz')
listToCuboid _ = error "Syntax error."

parse :: String -> [(State, Cuboid)]
parse = map (B.bimap
                  (read . toTitle)
                  (listToCuboid
                  . map (map read . splitOn ".." . drop 2)
                  . splitOn ",")
            . toTuple
            . splitOn " ")
        . lines
      where toTuple [a, b] = (a, b)
            toTuple _ = error "Syntax error."
            toTitle (n:ns) = toUpper n:ns
            toTitle [] = "Invalid can't use an empty list."

isValidCuboid :: Cuboid -> Bool
isValidCuboid n = min' (x n) < max' (x n) && 
                  min' (y n) < max' (y n) &&
                  min' (z n) < max' (z n)

splitLine :: Line -> Line -> [Line]
splitLine line line' = filter isValidLine [Line a b, Line b c, Line c d]
      where a = min (min' line) (min' line')
            b = max (min' line) (min' line')
            c = min (max' line) (max' line')
            d = max (max' line) (max' line')
            isValidLine n = min' n < max' n

listLineToCuboid :: [Line] -> Cuboid
listLineToCuboid [xx, yy, zz] = Cuboid xx yy zz
listLineToCuboid _ = error "Syntax error."

splitCube :: Cuboid -> Cuboid -> [Cuboid]
splitCube cube cube' = filter isValidCuboid
                       . map listLineToCuboid 
                       $ sequence [splitLine xx xx', splitLine yy yy', splitLine zz zz']
      where Cuboid xx yy zz = cube
            Cuboid xx' yy' zz' = cube' 

main :: IO ()
main = do
      input <- getContents
      print . length $ splitCube (Cuboid (Line 0 1) (Line 0 1) (Line 0 1)) (Cuboid (Line 2 3) (Line 2 3) (Line 2 3))