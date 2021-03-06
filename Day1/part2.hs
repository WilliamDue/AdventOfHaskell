toIntList :: String -> [Integer]
toIntList = fmap (\n -> read n :: Integer) . lines

main :: IO ()
main = do
      content <- getContents
      let list' = toIntList content
      let listSum = zipWith3 (\a b c -> a + b + c) list' (tail list') (tail $ tail list')
      print . length . filter id . zipWith (<) listSum $ tail listSum