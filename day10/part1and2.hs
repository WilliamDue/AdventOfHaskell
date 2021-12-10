import Data.List.Split ( splitOn, )
import Data.Maybe ( fromJust, isJust, isNothing, fromMaybe )
import Data.List ( sort ) 
import Debug.Trace ( traceShowId )


invert :: Char -> Char
invert ')' = '('
invert ']' = '['
invert '}' = '{'
invert '>' = '<'
invert '(' = ')'
invert '[' = ']'
invert '{' = '}'
invert '<' = '>'
invert n = n

errorCheck :: String -> String -> Maybe Char
errorCheck n [] = Nothing
errorCheck open (x:xs)
      | isClosing x && invert x == y = errorCheck ys xs
      | isOpening x = errorCheck (x:open) xs
      | otherwise = Just x
      where isClosing = flip elem [')', ']', '}', '>']
            isOpening = flip elem ['(', '[', '{', '<']
            y = head open
            ys = tail open


complete :: String -> String -> Maybe String
complete n [] = Just $ map invert n
complete open (x:xs)
      | isClosing x && invert x == y = complete ys xs
      | isOpening x = complete (x:open) xs
      | otherwise = Nothing
      where isClosing = flip elem [')', ']', '}', '>']
            isOpening = flip elem ['(', '[', '{', '<']
            y = head open
            ys = tail open


intRepr1 :: Char -> Int
intRepr1 ')' = 3
intRepr1 ']' = 57
intRepr1 '}' = 1197
intRepr1 '>' = 25137
intRepr1 _ = 0

intRepr2 :: Char -> Int
intRepr2 ')' = 1
intRepr2 ']' = 2
intRepr2 '}' = 3
intRepr2 '>' = 4
intRepr2 _ = 0

-- errorSum :: String -> Int
findErrors :: [String] -> [Char]
findErrors = fromMaybe [] . sequence . filter isJust . map (errorCheck [])

errorSum :: [Char] -> Int
errorSum = sum . map intRepr1

findCompletion :: [String] -> [String]
findCompletion = fromMaybe [] . sequence . filter isJust . map (complete [])

findCompletionScore :: [[Char]] -> [Int]
findCompletionScore = sort . map (foldl f 0)
      where f prev next = prev * 5 + intRepr2 next

findMiddle :: [a] -> a
findMiddle list' = list' !! n
      where n = length list' `div` 2

main :: IO ()
main = do
      content <- getContents
      print . errorSum . findErrors $ lines content
      print . findMiddle . findCompletionScore . findCompletion $ lines content