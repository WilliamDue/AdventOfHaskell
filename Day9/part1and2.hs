import Data.List.Split ( splitOn, )
import Data.Maybe ( fromJust, isJust, isNothing, fromMaybe )
import Debug.Trace
import qualified Data.Map as M
import qualified Data.List as L


toIntList :: [Char] -> [Int]
toIntList n = read $ "[" ++ L.intersperse ',' n ++ "]"

parse :: String -> M.Map (Int, Int) Int
parse = aux 0 M.empty . map toIntList . lines
      where aux _ map' [] = map'
            aux n map' (x:xs) = aux (n+1) map'' xs
                  where map'' = M.union map' . M.fromList $ zip [(n, x) | x <- [0..]] x


isLow :: M.Map (Int, Int) Int -> (Int, Int) -> Bool
isLow map' (x, y) = all (mid <) [up, down, left, right]
      where up = M.findWithDefault 10 (x - 1, y) map'
            down = M.findWithDefault 10 (x + 1, y) map'
            left = M.findWithDefault 10 (x, y - 1) map'
            right = M.findWithDefault 10 (x, y + 1) map'
            mid = fromJust $ M.lookup (x, y) map'


upF :: (Int, Int) -> (Int, Int)
upF (x, y) = (x - 1, y)

downF :: (Int, Int) -> (Int, Int)
downF (x, y) = (x + 1, y)

leftF :: (Int, Int) -> (Int, Int)
leftF (x, y) = (x, y - 1)

rightF :: (Int, Int) -> (Int, Int)
rightF (x, y) = (x, y + 1)

findBasin :: M.Map (Int, Int) Int -> Int -> (Int, Int) -> M.Map (Int, Int) Int
findBasin map' last cur
      | isNothing curLup = M.empty
      | last < curLup' && 9 /= curLup' = M.union result $ M.singleton cur curLup'
      | otherwise = M.empty 
            where curLup = M.lookup cur map'
                  curLup' = fromJust curLup

                  up = findBasin map' curLup' $ upF cur
                  down = findBasin map' curLup' $ downF cur
                  left = findBasin map' curLup' $ leftF cur
                  right = findBasin map' curLup' $ rightF cur

                  result = M.union up . M.union down . M.union left $ right

initFindBasin :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
initFindBasin map' key = M.union up . M.union down . M.union left . M.union right $ M.singleton key lup
      where lup = fromJust $ M.lookup key map'
            up = findBasin map' lup $ upF key
            down = findBasin map' lup $ downF key
            left = findBasin map' lup $ leftF key
            right = findBasin map' lup $ rightF key


findBasins :: M.Map (Int, Int) Int -> [M.Map (Int, Int) Int]
findBasins map' = map (initFindBasin map') low
      where low = map fst . M.toList $ M.filterWithKey (\k _ -> isLow map' k) map'


main :: IO ()
main = do
      content <- getContents
      let map' =  parse content
      print . sum . M.map (+1) $ M.filterWithKey (\k _ -> isLow map' k) map'
      print . product . take 3 . reverse . L.sort . map M.size $ findBasins map'