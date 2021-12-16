{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.List as L
import Data.Bifunctor ( second, first, bimap )
import Data.Maybe ( mapMaybe, fromJust, isNothing, isJust, maybe)
import Data.Foldable (minimumBy, Foldable (toList))
import Data.Function (on)
import Debug.Trace (traceShowId)
import Data.Array (Ix(range))

type Pos = (Int, Int)
type Graph = A.Array Pos Int
type Dist = A.Array Pos (Maybe Int)

toIntList :: String -> [Int]
toIntList = map (read . (:[]))

parse :: [Char] -> Graph
parse s = matrix
      where lines' = lines s
            s' = toIntList $ filter (/='\n') s
            maxY = length lines' - 1
            maxX = (length . head) lines' - 1
            matrix = A.listArray ((0, 0), (maxY, maxX)) s'

neighbours :: Graph -> Pos -> [Pos]
neighbours graph (x, y) = neighbours'
      where neighbours' = L.filter inbound [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
            inbound (a, b) = minX <= a && a <= maxX && minY <= b && b <= maxY
                  where ((minX, minY), (maxX, maxY)) = A.bounds graph

mkDist :: Graph -> Dist
mkDist = flip A.listArray (repeat Nothing) .  A.bounds

mkQueue :: Graph -> [Pos]
mkQueue = range . A.bounds

getMinQ :: [Pos] -> Dist -> Maybe (Pos, Int)
getMinQ q dist
      | L.null n = Nothing
      | otherwise = Just $ head n
      where n = L.sortOn snd
                . map (\n -> (n, fromJust $ dist A.! n))
                . filter (isJust . (dist A.!))
                $ q

updateDist :: Dist -> (Pos, Int) -> Maybe (Pos, Maybe Int)
updateDist dist (key, val) = dist'
      where dist' = case dist A.! key of
                        Nothing -> Just (key, Just val)
                        Just c -> if val < c
                                  then Just (key, Just val)
                                  else Nothing

dijkstra :: Pos -> Graph -> Dist
dijkstra source' graph' = aux graph' queue distance
      where distance' = mkDist graph'
            distance = distance' A.// [(source', Just 0)]
            queue = mkQueue graph'
            aux :: Graph -> [Pos] -> Dist -> Dist
            aux graph q dist
                  | null q = dist
                  | otherwise = aux graph q' $ dist A.// alt
                        where min' = getMinQ q dist
                              (key, val) = fromJust min'
                              q' = filter (/=key) q
                              alt = mapMaybe (updateDist dist . (\n -> (n, (val+) $ graph' A.! n)))
                                    . filter (`elem` q')
                                    $ neighbours graph key

modIndex :: Pos -> Graph -> (Pos, Int)
modIndex (y, x) graph = ((x, y),  if m' == 0 then 1 else m')
      where m = sum [y `div` (maxY + 1), x `div` (1 + maxX)]
            ((minY, minX), (maxY, maxX)) = A.bounds graph
            pos = (x `mod` (1 + maxX), y `mod` (maxY + 1))
            m' = 1 + (m + graph A.! pos - 1) `mod` 9

scale :: Int -> Graph -> Graph
scale s graph = graph' A.// [modIndex idx graph | idx <- A.indices graph']
      where ((minY, minX), (maxY, maxX)) = A.bounds graph
            bounds' = ((minY, minX), (s * (maxY + 1) - 1, s * (maxX + 1) - 1))
            graph' = A.listArray bounds' [0..]

solve :: Graph -> Int
solve graph = fromJust $ dijkstra head' graph A.! last'
      where last' = snd $ A.bounds graph
            head' = fst $ A.bounds graph

main :: IO ()
main = do
      input <- getContents
      let graph = parse input
      print $ solve graph
      print . solve $ scale 5 graph