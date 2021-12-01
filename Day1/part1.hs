import Data.Set ( fromList, toList )

toIntList :: String -> [Integer]
toIntList = fmap (\n -> read n :: Integer) . lines

main :: IO ()
main = do
      content <- getContents
      let list' = toIntList content
      print . length . filter (True==) . zipWith (<) list' $ tail list'