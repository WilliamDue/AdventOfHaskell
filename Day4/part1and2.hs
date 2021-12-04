import Data.List.Split ( splitOn )
import Data.List ( transpose )

type BingoBoard = [[(Integer, Bool)]]

toInt :: String -> Integer
toInt n = read n :: Integer

matrixParse :: String -> BingoBoard
matrixParse = map (map (\n -> (toInt n, False)) . words) . lines

parse :: String -> ([Integer], [BingoBoard])
parse s = (first, second)
      where terms = splitOn "\n\n" s
            first = map toInt . splitOn "," $ head terms
            second = map matrixParse $ tail terms

winCheck :: BingoBoard -> Bool
winCheck n = rowCheck n || colCheck n
      where rowCheck = any (all snd)
            colCheck = rowCheck . transpose

update :: Integer -> (Integer, Bool) -> (Integer, Bool)
update n (a, False)
      | n == a = (a, True)
      | otherwise = (a, False)
update _ n = n

updateMatrix :: Integer -> BingoBoard -> BingoBoard
updateMatrix n = map (map (update n))

sumMatrix :: BingoBoard -> Integer
sumMatrix = sum . map (sum . map fst . filter (not . snd))

playWin :: [Integer] -> [BingoBoard] -> Integer
playWin [] boards = -1
playWin (x:xs) boards
      | null winBoards = playWin xs updateBoards
      | otherwise = (*x) . sumMatrix $ head winBoards
      where updateBoards = map (updateMatrix x) boards
            winBoards = filter winCheck updateBoards

playLose :: [Integer] -> [BingoBoard] -> Integer
playLose [] boards = -1
playLose (x:xs) boards
      | null loseBoards = (*x) . sumMatrix $ head updateBoards
      | otherwise = playLose xs loseBoards
      where updateBoards = map (updateMatrix x) boards
            loseBoards = filter (not . winCheck) updateBoards

main :: IO ()
main = do
      content <- getContents
      print . uncurry playWin $ parse content
      print . uncurry playLose $ parse content