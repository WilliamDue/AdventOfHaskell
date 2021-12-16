{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.OrdPSQ as PQ
import Data.Bifunctor ( second, first, bimap )
import Data.Maybe ( mapMaybe, fromJust, isNothing, isJust, maybe, catMaybes)
import Data.Foldable (minimumBy, Foldable (toList))
import Data.Function (on)
import Debug.Trace (traceShowId)
import Data.Array (Ix(range))
import qualified Text.Html.BlockTable as PQ

type Pos = (Int, Int)
type Graph = A.Array Pos Int
type Neighbors = A.Array Pos [Pos]
type Queue = PQ.OrdPSQ Pos (Explored Int) (Explored Int)
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

mkQueue :: Graph -> Queue
mkQueue = PQ.fromList . map (\n -> (n, Inf, Inf)) . A.indices

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

multiInsert :: Queue -> [(Pos, Explored Int)] -> Queue
multiInsert q [] = q
multiInsert q ((a, b):xs) = multiInsert (PQ.insert a b b q) xs

dijkstra :: Pos -> Graph -> Dist
dijkstra source' graph' = aux graph' queue distance
      where distance' = mkDist graph'
            distance = distance' A.// [(source', Exist 0)]
            neighbours' = mkNeighbours graph'
            queue = PQ.insert source' (Exist 0) (Exist 0) $ mkQueue graph'
            aux :: Graph -> Queue -> Dist -> Dist
            aux graph q dist
                  | PQ.null q = dist
                  | isNothing min' = dist
                  | isInf value = dist
                  | otherwise = aux graph q'' $ dist A.// alt
                        where min' = PQ.findMin q
                              Just (key, value, _) = min'
                              val = case value of
                                          Exist n -> n
                                          Inf -> error "This can't happen" 
                              q' = PQ.deleteMin q
                              q'' = multiInsert q'
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