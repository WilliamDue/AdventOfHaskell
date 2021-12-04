import Data.List.Split ( splitOn )
import Data.List ( transpose )
import Data.Maybe ( catMaybes, fromJust )

type BingoBoard = [[Maybe Integer]]

bingoBoardParse :: String -> BingoBoard
bingoBoardParse = map (map (Just . toInt) . words) . lines
      where toInt n = read n :: Integer

parse :: String -> ([Integer], [BingoBoard])
parse s = (first, second)
      where toInt n = read n :: Integer
            terms = splitOn "\n\n" s
            first = map toInt . splitOn "," $ head terms
            second = map bingoBoardParse $ tail terms

winCheck :: BingoBoard -> Bool
winCheck n = rowCheck n || colCheck n
      where rowCheck = any (all (==Nothing))
            colCheck = rowCheck . transpose

updateBingoBoard :: Integer -> BingoBoard -> BingoBoard
updateBingoBoard n = map (map (update n))
      where update n (Just n') = if n == n' then Nothing else Just n'
            update n Nothing = Nothing

sumBingoBoard :: BingoBoard -> Integer
sumBingoBoard = sum . map (sum . catMaybes)

playWin :: [Integer] -> [BingoBoard] -> Maybe Integer
playWin [] boards = Nothing
playWin (x:xs) boards
      | null winBoards = playWin xs updateBoards
      | otherwise = Just . (*x) . sumBingoBoard $ head winBoards
      where updateBoards = map (updateBingoBoard x) boards
            winBoards = filter winCheck updateBoards

playLose :: [Integer] -> [BingoBoard] -> Maybe Integer
playLose [] boards = Nothing 
playLose (x:xs) boards
      | null loseBoards = Just . (*x) . sumBingoBoard $ head updateBoards
      | otherwise = playLose xs loseBoards
      where updateBoards = map (updateBingoBoard x) boards
            loseBoards = filter (not . winCheck) updateBoards

main :: IO ()
main = do
      content <- getContents
      print . fromJust . uncurry playWin $ parse content
      print . fromJust . uncurry playLose $ parse content