{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.List as L
import Data.Bifunctor ( second, first, bimap )
import Data.Maybe ( mapMaybe, fromJust, isNothing, isJust, maybe, catMaybes)
import Data.Foldable (minimumBy, Foldable (toList))
import Data.Function (on)
import Debug.Trace (traceShowId)
import Data.Array (Ix(range))

type Pos = (Int, Int)
type Graph = A.Array Pos Int
type Neighbors = A.Array Pos [Pos]
type Dist = A.Array Pos (Explored Int)

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

mkNeighbours :: Graph -> Neighbors
mkNeighbours graph = A.listArray bound $ map (neighbours graph) $ A.indices graph
      where bound = A.bounds graph

mkDist :: Graph -> Dist
mkDist = flip A.listArray (repeat Inf) .  A.bounds

mkQueue :: Graph -> M.Map Pos (Explored Int)
mkQueue = M.fromList . map (\n -> (n, Inf)) . A.indices

updateDist :: Dist -> (Pos, Int) -> Maybe (Pos, Explored Int)
updateDist dist (key, val) = dist'
      where dist' = case dist A.! key of
                        Inf -> Just (key, Exist val)
                        Exist c -> if val < c
                                  then Just (key, Exist val)
                                  else Nothing

data Explored n = Exist n | Inf deriving (Eq, Show)

instance (Eq n, Ord n) => Ord (Explored n) where
      Inf `compare` Exist _ = GT
      Exist _ `compare` Inf = LT
      Inf `compare` Inf = EQ
      Exist a `compare` Exist b = a `compare` b

instance Functor Explored where
      fmap f (Exist n) = Exist (f n)
      fmap f Inf = Inf

isInf :: Explored Int -> Bool
isInf Inf = True
isInf _ = False

dijkstra :: Pos -> Graph -> Dist
dijkstra source' graph' = aux graph' queue distance
      where distance' = mkDist graph'
            distance = distance' A.// [(source', Exist 0)]
            neighbours' = mkNeighbours graph'
            queue = M.insert source' (Exist 0) $ mkQueue graph'
            aux :: Graph -> M.Map Pos (Explored Int) -> Dist -> Dist
            aux graph q dist
                  | null q = dist
                  | isInf $ snd min' = dist
                  | otherwise = aux graph q'' $ dist A.// alt
                        where min' = minimumBy (compare `on` snd) $ M.toList q
                              (key, Exist val) = min'
                              q' = M.delete key q
                              q'' = flip M.union q'
                                    . M.fromList
                                    . filter (not . isInf . snd)
                                    $ alt
                              alt = mapMaybe (updateDist dist . (\n -> (n, (val+) $ graph' A.! n)))
                                    $ neighbours' A.! key

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

solve :: A.Array Pos Int -> Explored Int
solve graph = result A.! last'
      where last' = snd $ A.bounds graph
            head' = fst $ A.bounds graph
            result = dijkstra head' graph

main :: IO ()
main = do
      input <- getContents
      let graph = parse input
      print $ solve graph
      -- print . solve $ scale 5 graph