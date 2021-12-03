import Data.List ( transpose, partition, sortOn, maximumBy, sort )
import Data.Char( digitToInt )
import Control.Exception ( throw )
import Data.Bool(bool)


bin2dec :: [Bool] -> Int
bin2dec = foldl (\a -> (+) (2*a) . bool 0 1) 0


parse :: String -> Int
parse = bin2dec . map (=='1')


mostCommon :: Int -> [String] -> String
mostCommon _ [n] = n
mostCommon c nts
      | length zeroes <= length ones = mostCommon  (c+1) ones
      | otherwise = mostCommon  (c+1) zeroes
      where (ones, zeroes) = partition (\x -> x !! c == '1') nts


leastCommon :: Int -> [String] -> String
leastCommon _ [n] = n
leastCommon c nts
      | null zeroes && length ones > 1 = leastCommon (c+1) ones
      | null ones && length zeroes > 1 = leastCommon (c+1) zeroes
      | length zeroes > length ones = leastCommon (c+1) ones
      | otherwise = leastCommon  (c+1) zeroes
      where (ones, zeroes) = partition (\x -> x !! c == '1') nts


main :: IO ()
main = do
      content <- getContents
      let mCom = parse . mostCommon 0 $ lines content
      let lCom = parse .leastCommon 0 $ lines content
      print $ mCom * lCom