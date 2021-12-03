import Data.List ( transpose, partition, sortOn )
import Data.Char( digitToInt )
import Control.Exception ( throw )
import Data.Bool( bool )

program :: String -> Int
program = product
          . map (foldl (\a -> (+) (2*a) . bool 0 1) 0)
          . transpose
          . map (
               map ((==1) . simplify)
               . sortOn length
               . fromTuple 
               . partition (==0) 
               . map digitToInt
              ) 
          . transpose 
          . lines
          where fromTuple (a, b) = [a, b]
                simplify n = head n

main :: IO ()
main = do
      content <- getContents
      print $ program content