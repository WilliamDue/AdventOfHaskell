{-# LANGUAGE TupleSections #-}
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Control.Monad (forM_)
import Data.Maybe (fromJust, catMaybes, mapMaybe, isJust)
import Data.Array.IO

type Image = M.Map (Int, Int) Char
type Algorithm = A.Array Int Char

t = A.range ((0, 0), (1, 1))

parse :: String -> (Algorithm, Image)
parse input = (A.listArray (0, length alg - 1) alg, M.fromList img)
      where (alg, img') = (\(a:b:_) -> (a, b)) . splitOn "\n\n" $ input
            maxY = length . lines $ img'
            maxX = length . head . lines $ img'
            img = zip (A.range ((0, 0), (maxX - 1, maxY - 1)))
                  . filter (/='\n')
                  $ img'

bintodec :: [Bool] -> Int
bintodec = foldr (\x y -> fromEnum x + 2*y) 0 . reverse

neighboursToInt :: String -> Int
neighboursToInt n
      | length n == 9 = bintodec $ map (=='#') n
      | otherwise = error "Invalid input: string must be 9 chars"

getNeighbours :: Image -> (Int, Int) -> Maybe String
getNeighbours img (x, y)
      | length out' == 9 = Just out'
      | otherwise = Nothing
      where range = map (bimap (+x) (+y)) $ A.range ((-1, -1), (1, 1))
            out' = mapMaybe (`M.lookup` img) range

getImgBounds :: Image -> ((Int, Int), (Int, Int))
getImgBounds = minmax . unzip . M.keys
      where minmax (x, y) = ((minimum x, minimum y), (maximum x, maximum y))

expandImage :: Int -> Image -> Image
expandImage n img = M.union img
                    . M.fromList
                    . zip (A.range ((minX - n, minY - n), (maxX + n, maxY + n))) 
                    $ repeat '.'
      where ((minX, minY), (maxX, maxY)) = getImgBounds img

upScalePix :: Algorithm -> Image -> (Int, Int) -> Maybe Char
upScalePix alg img = fmap ((alg A.!) . neighboursToInt) . getNeighbours img

printImg :: Image -> IO ()
printImg img = do
      forM_ [minX .. maxX] $ \i -> do
            forM_ [minY .. maxY] $ \j -> do
                  putChar . fromJust $ M.lookup (i, j) img
            putStrLn ""
      where ((minX, minY), (maxX, maxY)) = getImgBounds img

upScale :: Algorithm -> Image -> Image
upScale alg img = M.fromList
                  . mapMaybe helper
                  . A.range
                  . getImgBounds
                  $ img
      where helper n = if isJust temp then Just (n, fromJust temp) else Nothing
                  where temp = upScalePix alg img n

solve1 :: String -> Int
solve1 str = length
             . filter (=='#')
             . M.elems
             . upScale alg
             . upScale alg
             . expandImage (3*2)
             $ img
      where (alg, img) = parse str

solve2 :: String -> Int
solve2 str = length
             . filter (=='#')
             . M.elems
             . last
             . take 51
             . iterate (upScale alg)
             . expandImage (3*50)
             $ img
      where (alg, img) = parse str

main :: IO ()
main = do
      input <- getContents
      let (alg, img) = parse input
      print . solve1 $ input
      print . solve2 $ input