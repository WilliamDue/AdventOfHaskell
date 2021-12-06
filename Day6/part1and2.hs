import Data.List.Split ( splitOn )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map


parse :: String -> [Int]
parse = map toInt . splitOn ","
      where toInt n = read n :: Int

func :: Map.Map Int Int -> Map.Map Int Int
func n = Map.unionWith (+) n'' mOne'
      where n' = Map.mapKeys (\k -> k - 1) n
            (mOne, n'') = Map.partitionWithKey (\k _ -> k == -1) n'
            mOnes = Map.findWithDefault 0 (-1) mOne
            mOne' = Map.fromList [(8, mOnes), (6, mOnes)]


toMap :: [Int] -> Map.Map Int Int
toMap = foldl1 (Map.unionWith (+)) . map (\n -> Map.fromList [(n, 1)])

count :: Map.Map Int Int -> Int
count = sum . map snd . Map.toList

main :: IO ()
main = do
      content <- getContents
      print . count . last . take 81 . iterate func . toMap $ parse content
      print . count . last . take 257 . iterate func . toMap $ parse content