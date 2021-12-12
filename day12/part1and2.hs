{-# LANGUAGE TupleSections #-}
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Data.Char (isLower, isUpper)
import Data.Graph.Inductive (neighbors)
import Data.Maybe (fromJust, fromMaybe)
import Distribution.Simple (UserHooks(preDoctest))

type Graph = (S.Set String, M.Map String (S.Set String))
type Path = [String]

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
      | otherwise = newPath : concatMap (findAllPaths1 visited'' newPath graph) neighbors
      where (nodes, edges) = graph
            newPath = lastPath ++ [rootLabel]
            neighbors = S.toList $ fromJust $ M.lookup rootLabel edges
            (uppercase, lowercase) = M.partitionWithKey (curry (all isUpper . fst)) visited
            uppercase' = M.map (const False) uppercase
            visited' = M.union lowercase uppercase'
            visited'' = M.insert rootLabel True visited'
            isVisited = fromMaybe False $ M.lookup rootLabel visited


findAllPaths2 :: M.Map String Int -> Path -> Graph -> String -> [Path]
findAllPaths2 visited lastPath graph rootLabel
      | null neighbors = [lastPath ++ [rootLabel]]
      | t >= 1 = [lastPath ++ [rootLabel]]
      | visitAmount >= 1 = [lastPath ++ [rootLabel]]
      | otherwise = newPath : concatMap (findAllPaths2 visited'' newPath graph) neighbors
      where (nodes, edges) = graph
            newPath = lastPath ++ [rootLabel]
            neighbors = S.toList $ fromJust $ M.lookup rootLabel edges
            (uppercase, lowercase) = M.partitionWithKey (curry (all isUpper . fst)) visited
            t = length $ M.filter (==2) lowercase
            uppercase' = M.map (const 0) uppercase
            visited' = M.union lowercase uppercase'
            visited''
                  | M.member rootLabel visited' = M.adjust (+1) rootLabel visited'
                  | otherwise = M.insert rootLabel 1 visited'
            visitAmount = fromMaybe 0 $ M.lookup rootLabel visited

solve1 :: [Char] -> Int
solve1 s = length . filter ((=="end") . last) $ findAllPaths1 M.empty [] graph "start"
      where graph = parse s

filterInvalid :: S.Set String -> [Path] -> [Path]
filterInvalid nodes = filter f 
      where pred k = k /= "end" && k /= "start" && all isLower k
            lowercase = S.filter pred nodes
            count m n = length $ filter (==n) m
            f m = (<=1) . length . filter id . map ((2==) . count m) $ S.toList lowercase

solve2 s = length result
      where graph = parse s
            pre = filter ((=="end") . last) $ findAllPaths2 M.empty [] graph "start"
            result = filterInvalid (fst graph) pre

main :: IO ()
main = do
      input <- getContents
      let x = solve1 input
      let y = solve2 input
      print x
      print y