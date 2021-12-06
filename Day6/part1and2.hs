import Data.List.Split ( splitOn )
import qualified Data.Map as Map


parse :: String -> [Int]
parse = map read . splitOn ","

func :: Map.Map Int Int -> Map.Map Int Int
func n = Map.unionWith (+) n' mOne'
      where (mOne, n') = Map.partitionWithKey (\k _ -> k == -1) $ Map.mapKeys (\k -> k - 1) n
            mOne' = (\m -> Map.fromList [(8, m), (6, m)]) $ Map.findWithDefault 0 (-1) mOne

countFishAtDay :: Int -> [Int] -> Int
countFishAtDay n = count . last . take (n + 1) . iterate func . toMap
      where toMap = foldl1 (Map.unionWith (+)) . map (\n -> Map.fromList [(n, 1)])
            count = sum . map snd . Map.toList

main :: IO ()
main = do
      content <- getContents
      print . countFishAtDay 80 $ parse content
      print . countFishAtDay 256 $ parse content