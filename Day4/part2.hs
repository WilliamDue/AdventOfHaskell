import Data.Array ( Array, listArray )
import Data.List.Split ( splitOn )
import Graphics.Rendering.OpenGL (auxBuffers)
import Data.List ( transpose )

toInt :: String -> Integer
toInt n = read n :: Integer


type BingoBoard = [[(Integer, Bool)]]


matrixParse :: String -> BingoBoard
matrixParse = map (map (\n -> (toInt n, False)) . words) . lines


parse :: String -> ([Integer], [BingoBoard])
parse s = (first, second)
      where terms = splitOn "\n\n" s
            first = map toInt . splitOn "," $ head terms
            second = map matrixParse $ tail terms

rowCheck :: BingoBoard -> Bool
rowCheck = any (all snd)

colCheck :: BingoBoard -> Bool
colCheck = rowCheck . transpose

winCheck :: BingoBoard -> Bool
winCheck n = rowCheck n || colCheck n

update :: Integer -> (Integer, Bool) -> (Integer, Bool)
update n (a, False)
      | n == a = (a, True)
      | otherwise = (a, False)
update _ n = n

updateMatrix :: Integer -> BingoBoard -> BingoBoard
updateMatrix n = map (map (update n))


sumMatrix :: BingoBoard -> Integer
sumMatrix = sum . map (sum . map fst . filter (not . snd))

play :: [Integer] -> [BingoBoard] -> Integer
play [] boards = -1
play (x:xs) boards
      | null loseBoards = (*x) . sumMatrix $ head updateBoards
      | otherwise = play xs loseBoards
      where updateBoards = map (updateMatrix x) boards
            loseBoards = filter (not . winCheck) updateBoards

main :: IO ()
main = do
      content <- getContents
      print . uncurry play $ parse content