import Data.Maybe ( fromMaybe )


clean :: String -> [(String, Integer)]
clean = fromMaybe [] . mapM (toTuple . words) . lines
            where toInt n = read n :: Integer
                  toTuple [a, b] = Just (a, toInt b)
                  toTuple x = Nothing

program :: [(String, Integer)] -> (Integer, Integer, Integer) -> Maybe (Integer, Integer)
program [] (horiz, depth, _) = Just (horiz, depth)
program ((movement, scaler):xs) (horiz, depth, aim)
      | movement == "down" = program xs (horiz, depth, aim + scaler)
      | movement == "up" = program xs (horiz, depth, aim - scaler)
      | movement == "forward" = program xs (horiz + scaler, depth + scaler * aim, aim)
      | otherwise = Nothing

main :: IO ()
main = do
      content <- getContents
      let list' = clean content
      print . fmap (uncurry (*))  $ program list' (0, 0, 0)