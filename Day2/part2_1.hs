import Data.Maybe ( fromMaybe )


clean :: String -> [(String, Integer)]
clean = fromMaybe [] . mapM (toTuple . words) . lines
            where toTuple [a, b] = Just (a, read b :: Integer)
                  toTuple _ = Nothing

func :: (String, Integer) -> ((Integer, Integer, Integer) -> (Integer, Integer, Integer))
func ("down", x) = \(horiz, depth, aim) -> (horiz, depth, aim + x)
func ("up", x) = \(horiz, depth, aim) -> (horiz, depth, aim - x)
func ("forward", x) = \(horiz, depth, aim) -> (horiz + x, depth + x * aim, aim)
func _ = id

program :: String -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
program = foldl (flip (.)) id . map func . clean

main :: IO ()
main = do
      content <- getContents
      print . (\(h, d, a) -> h * d) $ program content (0, 0, 0)