import Data.List.Split ( splitOn )
import Control.Monad ( replicateM )
import qualified Data.List as L
import Data.Bifunctor ( bimap )
import Data.Tuple (swap)


parse :: String -> (Int, Int)
parse str = (read x, read y)
      where x = last . splitOn ": " . head . lines $ str
            y = last . splitOn ": " . last . lines $ str

gauss3sum :: Int -> Int
gauss3sum n = 9 * n + 6

player1 :: Int -> Int
player1 n = gauss3sum $ 2*n

player2 :: Int -> Int
player2 n = gauss3sum $ 2*n+1

addHead :: (Int -> Int) -> [Int] -> [Int]
addHead f (x:xs) = f x:xs
addHead n [] = error "Invalid Input"

mod' :: Int -> Int
mod' m = (+1) $ (m - 1) `mod` 10

play1 :: Int -> [Int]
play1 a = scanl1 (+) . scanl1 (\x y -> mod' (x + y)) . addHead (mod' . (+a)) $ map player1 [0..]

play2 :: Int -> [Int]
play2 a = scanl1 (+) . scanl1 (\x y -> mod' (x + y)) . addHead (mod' . (+a)) $ map player2 [0..]

--solve1 :: String -> Int
solve1 str = (3 * length n + 3) * last n
      where (a, b) = parse str
            n = takeWhile (<1000) . concat $ zipWith (\a b -> [a, b]) (play1 a) (play2 b)

-- I basically stole kens code since i was clue less but i get the idea of it.

type Player = (Int, Int)

rollSums :: [[Int]]
rollSums = L.group . L.sort . map sum . replicateM 3 $ [1, 2, 3]

groupFunc :: (Player, Player) -> [Int] -> (Int, Int)
groupFunc ((score, pos), player2) group
      | score' >= 21 = (n, 0)
      | otherwise = bimap (*n) (*n) . swap . countWins $ (player2, (score', pos'))
      where n = length group
            pos' = mod' $ pos + head group
            score' = score + pos'

countWins :: (Player, Player) -> (Int, Int)
countWins players = bimap sum sum . unzip . map (groupFunc players) $ rollSums

solve2 :: String -> Int
solve2 = uncurry max . countWins . (\(p1, p2) -> ((0, p1), (0, p2))) . parse

main :: IO ()
main = do
      input <- getContents
      print . solve1 $ input
      print . solve2 $ input