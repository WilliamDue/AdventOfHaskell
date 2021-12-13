{-# LANGUAGE TupleSections #-}
import qualified Data.List as L
import qualified Data.Set as S
import Data.List.Split ( splitOn )

toOper :: [String] -> (Char, Int)
toOper [a, b] = (head a, read b)
toOper _ = error "input error"

toTuple :: Show b => [b] -> (b, b)
toTuple [a, b] = (a, b)
toTuple n = error $ "input error " ++ show n

splitDoubleLine :: String -> (String, String)
splitDoubleLine = toTuple . splitOn "\n\n"

parse :: String -> ([(Char, Int)], [(Int, Int)])
parse blob = (oper, paper)
      where (dots, folds) = splitDoubleLine blob
            oper = map ((toOper . splitOn "=") . (last . splitOn "along ")) $ lines folds
            paper = map (toTuple . map read . splitOn ",") $ lines dots

foldPaper :: (Char, Int) -> (Int, Int) -> (Int, Int)
foldPaper ('y', n) (x, y) = (x, abs $ y - 2*n)
foldPaper ('x', n) (x, y) = (abs $ x - 2*n, y)
foldPaper (_, _) (x, y) = (x, y)

partitionBy :: (Char, Int) -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
partitionBy ('y', n) paper = L.partition ((n<=) . snd) paper
partitionBy ('x', n) paper = L.partition ((n<=) . fst) paper
partitionBy (_, _) paper = error "invalid input"

union :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
union a b = S.toList . S.fromList $ L.union a b

applyFold :: (Char, Int) -> [(Int, Int)] -> [(Int, Int)]
applyFold oper paper = L.sort $ high' `union` low
      where (high, low) = partitionBy oper paper
            high' = map (foldPaper oper) high

applyNFold :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
applyNFold opers paper  = foldl (flip applyFold) paper opers

main :: IO ()
main = do
      input <- getContents
      let (opers, paper) = parse input
      let part1 = applyFold (head opers) paper
      print $ length part1
      let temp = applyNFold opers paper
      let maxX = maximum $ map fst temp
      let maxY = maximum $ map snd temp
      let result = temp
      print result