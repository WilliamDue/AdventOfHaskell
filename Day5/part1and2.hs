import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe )
import qualified Data.Map as Map

type Point = (Int, Int)
type LineSegment = (Point, Point)

toLineSeg :: [[Int]] -> Maybe LineSegment
toLineSeg [[x0, y0], [x1, y1]] = Just ((x0, y0), (x1, y1))
toLineSeg _ = Nothing

parse :: String -> [LineSegment]
parse = mapMaybe (toLineSeg . map (map toInt . splitOn ",") . splitOn " -> ") . lines
      where toInt n = read n :: Int

limits :: LineSegment -> LineSegment
limits ((x0, y0), (x1, y1)) = ((minX, minY), (maxX, maxY))
      where minY = min y0 y1
            maxY = max y0 y1
            minX = min x0 x1
            maxX = max x0 x1

addPlot :: Map.Map (Int, Int) Int -> LineSegment -> Map.Map (Int, Int) Int
addPlot map' ((x0, y0), (x1, y1))
      | minX - maxX == minY - maxY = Map.unionWith (+) map' $ Map.fromList diag
      | x0 == x1 = Map.unionWith (+) map' $ Map.fromList [((x0, y), 1) | y <- [minY .. maxY]]
      | y0 == y1 = Map.unionWith (+) map' $ Map.fromList [((x, y0), 1) | x <- [minX .. maxX]]
      | otherwise = map'
      where ((minX, minY), (maxX, maxY)) = limits ((x0, y0), (x1, y1))
            a = (y0 - y1) `div` (x0 - x1)
            b = y0 - a * x0
            diag = [((x, a * x + b), 1) | x <- [minX .. maxX]]

intersectsPart1 :: [LineSegment] -> Int
intersectsPart1 = Map.size . Map.filter (2<=) . foldl addPlot Map.empty . filter segPred
      where segPred ((x0, y0), (x1, y1)) = x0 == x1 || y0 == y1

intersectsPart2 :: [LineSegment] -> Int
intersectsPart2 = Map.size . Map.filter (2<=) . foldl addPlot Map.empty

main :: IO ()
main = do
      content <- getContents
      print . intersectsPart1 $ parse content
      print . intersectsPart2 $ parse content