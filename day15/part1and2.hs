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

test :: String
test = "1163751742\
       \1381373672\
       \2136511328\
       \3694931569\
       \7463417111\
       \1319128137\
       \1359912421\
       \3125421639\
       \1293138521\
       \2311944581"

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
            matrix = A.listArray ((0, 0), (maxX, maxY)) s'

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


solve1 :: Graph -> Int
solve1 graph = fromJust $ dijkstra head' graph A.! last'
      where last' = snd $ A.bounds graph
            head' = fst $ A.bounds graph

main :: IO ()
main = do
      input <- getContents
      print . solve1 $ parse input