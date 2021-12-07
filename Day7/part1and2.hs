import Data.List.Split ( splitOn )
import qualified Data.Map as Map


parse :: String -> [Int]
parse = map read . splitOn ","

posFuel :: Int -> [Int] -> Int
posFuel p = sum . map (abs . (p-))

minFuel :: [Int] -> Int
minFuel n = minimum $ map (`posFuel` n) [0..maxPos]
      where maxPos = maximum n


main :: IO ()
main = do
      content <- getContents
      print . minFuel $ parse content