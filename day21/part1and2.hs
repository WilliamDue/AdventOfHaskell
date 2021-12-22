import Data.List.Split ( splitOn )
import Control.Monad ( replicateM )
import Data.List ( partition ) 
import Data.Bifunctor ( bimap )


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

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

--solve1 :: String -> Int
solve1 str = (3 * length n + 3) * last n
      where (a, b) = parse str
            n = takeWhile (<1000) . concat $ zipWith (\a b -> [a, b]) (play1 a) (play2 b)

--play1' :: Int -> Int -> [Int]
play1' a b = map (scanl1 (+) . scanl1 (\x y -> mod' (x + y)) . addHead (mod' . (+a)) . map player1) $ replicateM b [1, 2, 3]

play2' a b = map (scanl1 (+) . scanl1 (\x y -> mod' (x + y)) . addHead (mod' . (+a)) . map player2) $ replicateM b [1, 2, 3]

test2 m str = bimap length length $ partition (even . length) n
      where (a, b) = parse str
            n = map (takeWhile (<21) . concat) $ zipWith (zipWith (\a b -> [a, b])) (play1' a m) (play2' b m)

main :: IO ()
main = do
      input <- getContents
      print . solve1 $ input
      print $ test2 16 input