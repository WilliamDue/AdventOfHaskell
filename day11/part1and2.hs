{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import qualified Data.List as L
import Data.List.Split


type Point = (Int, Int)

prettyPrint :: M.Map Point Int -> IO ()
prettyPrint map' = putStrLn . unlines . map show $ chunksOf (1 + max') elem'
      where keys' = L.sort $ M.keys map'
            max' = L.maximum $ map snd keys'
            elem' = M.elems map'

toIntList :: String -> [Int]
toIntList = read .  (++"]") . ("["++) . L.intersperse ','

toConvertible :: Int -> [[Int]] -> [(Point, Int)]
toConvertible c [] = []
toConvertible c (x:xs) = zip [(c, y) | y <- [0..]] x ++ toConvertible (c + 1) xs

parse :: String -> M.Map Point Int
parse = M.fromList . toConvertible 0 . map toIntList . lines

addOne :: M.Map Point Int -> M.Map Point Int
addOne = M.map (+1)

toZero :: M.Map Point Int -> M.Map Point Int
toZero = M.map aux
      where aux n
                  | 9 < n = 0
                  | otherwise = n

neighbours :: M.Map Point Int -> Point -> [Point]
neighbours map' (x, y) = points'
      where points = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                        (x, y - 1),                 (x, y + 1),
                      (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
            points' = filter (`elem` M.keys map') points

getGT9Keys :: M.Map Point Int -> [Point]
getGT9Keys = M.keys . M.filter (9 <)

toOneMap :: [Point] -> M.Map Point Int
toOneMap = M.fromList . map  (\n -> (n, 1))

findUpdates :: M.Map Point Int -> M.Map Point Int
findUpdates map'' = aux [] map'' emptyMap' 
      where emptyMap' = M.map (const 0) map''
            aux blacklist map' updates
                  | null keys' = updates
                  | otherwise = aux blacklist' map''' updates''
                  where blacklist' = getGT9Keys map'
                        keys' = blacklist' L.\\ blacklist
                        emptyMap = M.map (const 0) map'
                        updates' = foldl (M.unionWith (+)) emptyMap 
                              $ map (toOneMap . neighbours map') keys'
                        map''' = M.unionWith (+) map' updates'
                        updates'' = M.unionWith (+) updates updates'

updateLight :: M.Map Point Int -> (Int, M.Map Point Int)
updateLight map' = (numOfUpdates, map'')
      where updates = findUpdates map'
            map'' = toZero $ M.unionWith (+) updates map'
            numOfUpdates = length $ M.filter (==0) map''

nUpdateLight :: Int -> M.Map Point Int -> (Int, M.Map Point Int)
nUpdateLight n map' = (!!n) $ iterate aux (0, map')
      where aux (x, y) = (x + x', y')
                  where (x', y') = updateLight $ addOne y

simFlash :: M.Map Point Int -> (Int, M.Map Point Int, Int)
simFlash map' = head . dropWhile ((/=0) . sum . snd') $ iterate aux (0, map', 0)
      where thrd (_, _, a) = a
            snd' (_, a, _) = a
            aux (x, y, z) = (x + x', y', z + 1)
                  where (x', y') = updateLight $ addOne y

main :: IO ()
main = do
      content <- getContents
      let map' = parse content
      let part1 = nUpdateLight 100 map'
      prettyPrint $ snd part1
      print $ fst part1
      let (flashes, map'', step) = simFlash map'
      putStrLn ""
      prettyPrint map''
      print step