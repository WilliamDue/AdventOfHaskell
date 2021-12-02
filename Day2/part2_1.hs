import Data.Maybe ( fromMaybe )


func :: (String, Integer) -> ((Integer, Integer, Integer) -> (Integer, Integer, Integer))
func ("down", x) = \(horiz, depth, aim) -> (horiz, depth, aim + x)
func ("up", x) = \(horiz, depth, aim) -> (horiz, depth, aim - x)
func ("forward", x) = \(horiz, depth, aim) -> (horiz + x, depth + x * aim, aim)
func _ = id

parse :: String -> [(Integer, Integer, Integer) -> (Integer, Integer, Integer)]
parse = fromMaybe [] . mapM (fmap func . format . words) . lines
            where format [a, b] = Just (a, read b :: Integer)
                  format _ = Nothing

program :: String -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
program = foldl1 (flip (.)) . parse

main :: IO ()
main = do
      content <- getContents
      print . (\(h, d, a) -> h * d) $ program content (0, 0, 0)