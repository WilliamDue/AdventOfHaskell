import Data.Maybe ( fromMaybe )


func :: (Integer, Integer, Integer) -> (String, Integer) -> (Integer, Integer, Integer)
func (horiz, depth, aim) ("down", x) = (horiz, depth, aim + x)
func (horiz, depth, aim) ("up", x) = (horiz, depth, aim - x)
func (horiz, depth, aim) ("forward", x) = (horiz + x, depth + x * aim, aim)
func n _ = n

parse :: String -> [(String, Integer)]
parse = fromMaybe [] . mapM (format . words) . lines
            where format [a, b] = Just (a, read b :: Integer)
                  format _ = Nothing

program :: String -> (Integer, Integer, Integer)
program = foldl func (0, 0, 0) . parse

main :: IO ()
main = do
      content <- getContents
      print . (\(h, d, a) -> h * d) $ program content