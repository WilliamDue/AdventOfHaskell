{-# LANGUAGE TupleSections #-}
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Data.Char (isLower, isUpper)
import Data.Graph.Inductive (neighbors)
import Data.Maybe (fromJust, fromMaybe)

input :: IO String
input = getContents

missingEdges :: [(String, String)] -> [(String, String)]
missingEdges = concatMap aux
      where aux (a, b)
                  | a == "start" = [(a, b)]
                  | b == "end" = [(a, b)]
                  | b == "start" = [(b, a)]
                  | a == "end" = [(b, a)]
                  | otherwise = [(a, b), (b, a)]

toTuple :: [b] -> (b, b)
toTuple [a, b] = (a, b)
toTuple _ = error "Invalid input."

toList' :: (a, a) -> [a]
toList' (a, b) = [a, b]

type Graph = (S.Set String, M.Map String (S.Set String))
type Path = [String]

parse :: String -> Graph
parse text = (nodes, edges)
      where edges' = map (toTuple . splitOn "-") $ lines text
            edges'' = missingEdges edges'
            nodes = S.fromList $ concatMap toList' edges''
            edges = M.fromList .
                    map (\n -> (n,  S.fromList . map snd $ filter ((n==) . fst) edges''))
                    $ S.toList nodes

findAllPaths1 :: M.Map String Bool -> Path -> Graph -> String -> [Path]
findAllPaths1 visited lastPath graph rootLabel
      | null neighbors = [lastPath ++ [rootLabel]]
      | isVisited = [lastPath ++ [rootLabel]]
      | otherwise = newPath : concatMap (findAllPaths1 visited' newPath graph) neighbors
      where (nodes, edges) = graph
            newPath = lastPath ++ [rootLabel]
            neighbors = S.toList $ fromJust $ M.lookup rootLabel edges
            (uppercase, lowercase) = M.partitionWithKey (curry (all isUpper . fst)) visited
            uppercase' = M.map (const False) uppercase 
            visited' = M.union lowercase $ M.insert rootLabel True uppercase'
            isVisited = fromMaybe False $ M.lookup rootLabel visited


findAllPaths2 :: M.Map String Bool -> Path -> Graph -> String -> [Path]
findAllPaths2 visited lastPath graph rootLabel
      | null neighbors = [lastPath ++ [rootLabel]]
      | isVisited = [lastPath ++ [rootLabel]]
      | otherwise = newPath : concatMap (findAllPaths2 visited' newPath graph) neighbors
      where (nodes, edges) = graph
            newPath = lastPath ++ [rootLabel]
            neighbors = S.toList $ fromJust $ M.lookup rootLabel edges
            (uppercase, lowercase) = M.partitionWithKey (curry (all isUpper . fst)) visited
            uppercase' = M.map (const False) uppercase 
            visited' = M.union lowercase $ M.insert rootLabel True uppercase'
            isVisited = fromMaybe False $ M.lookup rootLabel visited

solve1 :: [Char] -> Int
solve1 s = length . filter ((=="end") . last) $ findAllPaths1 M.empty [] graph "start"
      where graph = parse s


solve2 s = length . filter ((=="end") . last) $ findAllPaths2 M.empty [] graph "start"
      where graph = parse s

answer1 :: IO Int
answer1 = solve1 <$> input


answer2 = solve2 <$> input

main :: IO ()
main = do
      x <- answer1
      print x