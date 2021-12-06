import Data.List.Split ( splitOn )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map


parse :: String -> [Int]
parse = map read . splitOn ","

func :: Map.Map Int Int -> Map.Map Int Int
func n = Map.unionWith (+) n' mOne'
      where (mOne, n') = Map.partitionWithKey (\k _ -> k == -1) $ Map.mapKeys (\k -> k - 1) n
            mOne' = (\m -> Map.fromList [(8, m), (6, m)]) $ Map.findWithDefault 0 (-1) mOne

toMap :: [Int] -> Map.Map Int Int
toMap = foldl1 (Map.unionWith (+)) . map (\n -> Map.fromList [(n, 1)])

count :: Map.Map Int Int -> Int
count = sum . map snd . Map.toList

main :: IO ()
main = do
      content <- getContents
      print . count . last . take 81 . iterate func . toMap $ parse content
      print . count . last . take 257 . iterate func . toMap $ parse content