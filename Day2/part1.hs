import Data.Maybe ( fromMaybe )


clean :: String -> [(String, Integer)]
clean = fromMaybe [] . mapM (toTuple . words) . lines
            where toInt n = read n :: Integer
                  toTuple [a, b] = Just (a, toInt b)
                  toTuple x = Nothing


program :: [(String, Integer)] -> (Integer, Integer) -> Maybe (Integer, Integer)
program [] (a, b) = Just (a, b)
program (x:xs) (a, b)
      | movement == "forward" = program xs (a + scaler, b)
      | movement == "down" = program xs (a, b + scaler)
      | movement == "up" = program xs (a, b - scaler)
      | otherwise = Nothing
      where movement = fst x
            scaler = snd x


main :: IO ()
main = do
      content <- getContents
      let list' = clean content
      print . fmap (uncurry (*)) $ program list' (0, 0)