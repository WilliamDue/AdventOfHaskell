import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe )
import qualified Data.Map as Map


parse :: String -> [Int]
parse = map toInt . splitOn ","
      where toInt n = read n :: Int

func :: Int -> [Int]
func n = if n - 1 == -1 then [6, 8] else [n - 1]

func' :: Map.Map Int Int -> Map.Map Int Int
func' n = Map.unionWith (+) n' mOne'
      where (mOne, n') = Map.partitionWithKey (\k _ -> k == -1) $ Map.mapKeys (\k -> k - 1) n
            mOne' = Map.fromList [(6, Map.findWithDefault 0 -1 mOne), (8, Map.findWithDefault 0 -1 mOne)]


toMap :: [Int] -> Map.Map Int Int
toMap = foldl1 (Map.unionWith (+)) . map (\n -> Map.fromList [(n, 1)])

main :: IO ()
main = do
      content <- getContents
      -- print . length . last . take 81 . iterate (concatMap func) $ parse content
      print . func' . func' . toMap $ parse content