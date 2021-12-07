import Data.List.Split ( splitOn )


parse :: String -> [Int]
parse = map read . splitOn ","

posFuelPart1 :: Int -> [Int] -> Int
posFuelPart1 p = sum . map (abs . (p-))

posFuelPart2 :: Int -> [Int] -> Int
posFuelPart2 p = sum . map (tri . abs . (p-))
      where tri n = n * (n + 1) `div` 2

minFuel :: (Int -> [Int] -> Int) -> [Int] -> Int
minFuel posFuel n = minimum $ map (`posFuel` n) [0..maxPos]
      where maxPos = maximum n

main :: IO ()
main = do
      content <- getContents
      print . minFuel posFuelPart1 $ parse content
      print . minFuel posFuelPart2 $ parse content