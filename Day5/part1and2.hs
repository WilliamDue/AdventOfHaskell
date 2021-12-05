import Data.List.Split ( splitOn )
import Data.List ( transpose )
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

type Point = (Int, Int)
type LineSegment = (Point, Point)

toLineSeg :: [[Int]] -> Maybe LineSegment
toLineSeg [[x0, y0], [x1, y1]] = Just ((x0, y0), (x1, y1))
toLineSeg _ = Nothing

-- parse :: String -> [LineSegment]
parse :: String -> [LineSegment]
parse = mapMaybe (toLineSeg . map (map toInt . splitOn ",") . splitOn " -> ") . lines
      where toInt n = read n :: Int


borders :: LineSegment -> LineSegment -> LineSegment
borders n n' = ((minX, minY), (maxX, maxY))
      where ((x0, y0), (x1, y1)) = n
            ((x0', y0'), (x1', y1')) = n'
            minX = min x0 x0'
            maxX = max x1 x1'
            minY = min y0 y0'
            maxY = max x1 y1'


addPlot :: Map.Map (Int, Int) Int -> LineSegment -> Map.Map (Int, Int) Int
addPlot map' ((x0, y0), (x1, y1))
      | x0 == x1 = Map.unionWith (+) map' $ Map.fromList [((x0, y), 1) | y <- [minY .. maxY]]
      | y0 == y1 = Map.unionWith (+) map' $ Map.fromList [((x, y0), 1) | x <- [minX .. maxX]]
      | otherwise = map'
      where minY = min y0 y1
            maxY = max y0 y1
            minX = min x0 x1
            maxX = max x0 x1

segPart1Filter :: LineSegment -> Bool
segPart1Filter ((x0, y0), (x1, y1)) = x0 == x1 || y0 == y1

intersects :: [LineSegment] -> Int
intersects = Map.size . Map.filter (2<=) . foldl addPlot Map.empty . filter segPart1Filter

segPart2Filter :: LineSegment -> Bool
segPart2Filter ((x0, y0), (x1, y1)) = x0 == x1 || y0 == y1 || x0 - x1 == y0 - y1

main :: IO ()
main = do
      content <- getContents
      print . intersects $ parse content
      -- print . filter segPart2Filter $ parse content