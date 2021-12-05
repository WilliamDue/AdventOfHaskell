import Data.List.Split ( splitOn )
import Data.List ( transpose )
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as Map

type Point = (Int, Int)
type LineSegment = (Point, Point)

toLineSeg :: [[Int]] -> Maybe LineSegment
toLineSeg [[x0, y0], [x1, y1]] = Just ((x0, y0), (x1, y1))
toLineSeg _ = Nothing

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

addPlotPart1 :: Map.Map (Int, Int) Int -> LineSegment -> Map.Map (Int, Int) Int
addPlotPart1 map' ((x0, y0), (x1, y1))
      | x0 == x1 = Map.unionWith (+) map' $ Map.fromList [((x0, y), 1) | y <- [minY .. maxY]]
      | y0 == y1 = Map.unionWith (+) map' $ Map.fromList [((x, y0), 1) | x <- [minX .. maxX]]
      | otherwise = map'
      where minY = min y0 y1
            maxY = max y0 y1
            minX = min x0 x1
            maxX = max x0 x1

addPlotPart2 :: Map.Map (Int, Int) Int -> LineSegment -> Map.Map (Int, Int) Int
addPlotPart2 map' ((x0, y0), (x1, y1))
      | minX - maxX == minY - maxY = Map.unionWith (+) map' $ Map.fromList diag
      | x0 == x1 = Map.unionWith (+) map' $ Map.fromList [((x0, y), 1) | y <- [minY .. maxY]]
      | y0 == y1 = Map.unionWith (+) map' $ Map.fromList [((x, y0), 1) | x <- [minX .. maxX]]
      | otherwise = map'
      where minX = min x0 x1
            maxX = max x0 x1
            minY = min y0 y1
            maxY = max y0 y1
            a = (y0 - y1) `div` (x0 - x1)
            b = y0 - a * x0
            diag = [((x, a * x + b), 1) | x <- [minX .. maxX]]

intersectsPart1 :: [LineSegment] -> Int
intersectsPart1 = Map.size . Map.filter (2<=) . foldl addPlotPart1 Map.empty

intersectsPart2 :: [LineSegment] -> Int
intersectsPart2 = Map.size . Map.filter (2<=) . foldl addPlotPart2 Map.empty

main :: IO ()
main = do
      content <- getContents
      print . intersectsPart1 $ parse content
      print . intersectsPart2 $ parse content