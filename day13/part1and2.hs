{-# LANGUAGE TupleSections #-}
import qualified Data.Set as S
import Data.List.Split ( splitOn, chunksOf )

toOper :: [String] -> (Char, Int)
toOper [a, b] = (head a, read b)
toOper _ = error "input error"

toTuple :: Show b => [b] -> (b, b)
toTuple [a, b] = (a, b)
toTuple n = error $ "input error " ++ show n

splitDoubleLine :: String -> (String, String)
splitDoubleLine = toTuple . splitOn "\n\n"

parse :: String -> ([(Char, Int)], S.Set (Int, Int))
parse blob = (oper, paper)
      where (dots, folds) = splitDoubleLine blob
            oper = map ((toOper . splitOn "=") . (last . splitOn "along ")) $ lines folds
            paper = S.fromList . map (toTuple . map read . splitOn ",") $ lines dots

foldPaper :: (Char, Int) -> (Int, Int) -> (Int, Int)
foldPaper ('y', n) (x, y) = (x, abs $ y - 2*n)
foldPaper ('x', n) (x, y) = (abs $ x - 2*n, y)
foldPaper (_, _) (x, y) = (x, y)

partitionBy :: (Char, Int) -> S.Set (Int, Int) -> (S.Set (Int, Int), S.Set (Int, Int))
partitionBy ('y', n) paper = S.partition ((n<=) . snd) paper
partitionBy ('x', n) paper = S.partition ((n<=) . fst) paper
partitionBy (_, _) paper = error "invalid input"

applyFold :: (Char, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
applyFold oper paper = high' `S.union` low
      where (high, low) = partitionBy oper paper
            high' = S.map (foldPaper oper) high

applyNFold :: [(Char, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
applyNFold opers paper  = foldl (flip applyFold) paper opers

prettyPrint :: S.Set (Int, Int) -> IO ()
prettyPrint n = putStrLn $ unlines canvas''
      where n' = S.toList n
            maxX = S.findMax $ S.map fst n
            maxY = S.findMax $ S.map snd n
            canvas = [(x, y) | y <- [0..maxY], x <- [0..maxX]]
            canvas' = map (\k -> if k `elem` n' then '#' else ' ') canvas
            canvas'' = chunksOf (maxX + 1) canvas'

main :: IO ()
main = do
      input <- getContents
      let (opers, paper) = parse input
      let part1 = applyFold (head opers) paper
      print $ length part1
      let part2 = applyNFold opers paper
      prettyPrint part2