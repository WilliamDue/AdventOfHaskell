import Data.List.Split (splitOn)
import qualified Data.List as L
import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromJust, mapMaybe, isNothing)

data Path =  L | R deriving (Show, Eq, Ord)
data ShellFishNum n = End n | Number (ShellFishNum n) (ShellFishNum n) deriving (Show, Eq)

toEnd :: String -> (ShellFishNum Int, String)
toEnd str = (num, str')
      where num = End . read . takeWhile isDigit $ str
            str' = dropWhile isDigit str

toNumber :: String -> (ShellFishNum Int, String)
toNumber str = (Number left right, drop 1 str')
      where (left, tail') = parse' . drop 1 $ str
            (right, str') = parse' . drop 1 $ tail'

parse' :: String -> (ShellFishNum Int, String)
parse' n
      | isDigit $ head n = toEnd n
      | otherwise = toNumber n

parse :: String -> [ShellFishNum Int]
parse = map (fst . parse') . lines

findExplode' :: Int -> [Path] -> ShellFishNum Int -> [Maybe [Path]]
findExplode' n path (Number (End a) (End b))
      | 4 <= n = [Just path]
      | otherwise = [Nothing]
findExplode' n path (Number (Number a b) (End c)) = findExplode' (n+1) (L:path) (Number a b)
findExplode' n path (Number (End a) (Number b c)) = findExplode' (n+1) (R:path) (Number b c)
findExplode' n path (Number (Number a b) (Number c d)) = left ++ right
      where left = findExplode' (n+1) (L:path) (Number a b)
            right = findExplode' (n+1) (R:path) (Number c d)
findExplode' _ path _ = [Nothing]

findExplode :: ShellFishNum Int -> Maybe [Path]
findExplode n
      | null result = Nothing
      | otherwise = Just $ minimum result
      where result = map reverse . catMaybes . findExplode' 0 [] $ n

isNumber :: ShellFishNum a -> Bool
isNumber (Number _ _) = True
isNumber (End _) = True

takeLeft :: ShellFishNum a -> ShellFishNum a
takeLeft (Number a _) = a
takeLeft _ = error "Can't take left."

takeRight :: ShellFishNum a -> ShellFishNum a
takeRight (Number _ a) = a
takeRight _ = error "Can't take right."

isEnd :: ShellFishNum a -> Bool
isEnd (End _) = True
isEnd (Number _ _) = False

isValidPath :: ShellFishNum Int -> [Path] -> Bool
isValidPath _ [] = True
isValidPath tree (x:xs)
      | isEnd tree = False
      | x == L = isValidPath treeL xs
      | otherwise = isValidPath treeR xs
            where treeL = takeLeft tree
                  treeR = takeRight tree

getDepth :: ShellFishNum Int -> Int
getDepth = aux 0
      where aux n (Number (End a) (End b)) = n
            aux n (Number a b) = max left right
                  where left = aux (n+1) a
                        right = aux (n+1) b
            aux n _ = n

lookupByPath :: ShellFishNum Int -> [Path] -> ShellFishNum Int
lookupByPath tree [] = tree
lookupByPath (Number n _) (L:xs) = lookupByPath n xs
lookupByPath (Number _ n) (R:xs) = lookupByPath n xs
lookupByPath (End n) (x:xs)
      | null xs = End n
      | otherwise = error "Invalid Path"

invert :: Path -> Path
invert L = R
invert R = L

getNeighbourPath :: ShellFishNum Int -> Path -> [Path] -> Maybe [Path]
getNeighbourPath tree p path
      | null temp = Nothing
      | null result = Nothing
      | otherwise = Just . head $ result
      where result = dropWhile (not . isValidPath tree)
                     . map reverse
                     . zipWith drop [0..]
                     . replicate (length path)
                     . reverse
                     . take (length path + 1)
                     . (++ (repeat $ invert p))
                     . reverse
                     . (p:)
                     . tail
                     $ temp
            temp = dropWhile (==p)
                   . reverse
                   $ path

insertByPath :: ShellFishNum Int -> [Path] -> ShellFishNum Int -> ShellFishNum Int
insertByPath tree [] m = m
insertByPath (Number n k) (L:xs) m = Number (insertByPath n xs m) k
insertByPath (Number k n) (R:xs) m = Number k (insertByPath n xs m)
insertByPath (End n) _ _ = error "Invalid Path"

insertManyByPath :: ShellFishNum Int -> [([Path], ShellFishNum Int)] -> ShellFishNum Int
insertManyByPath tree [] = tree
insertManyByPath tree ((path, newTree):xs) = insertManyByPath tree' xs
      where tree' = insertByPath tree path newTree

explode :: ShellFishNum Int -> ShellFishNum Int
explode tree
      | isNothing path' = tree
      | otherwise = insertManyByPath tree . catMaybes . aux tree . fromJust $ path'
      where path' = findExplode tree
            aux tree' path = [left, Just (path, End 0), right]
                  where (Number (End a) (End b)) = lookupByPath tree' path
                        left = case getNeighbourPath tree' L path of
                                    Nothing -> Nothing
                                    Just path' -> case lookupByPath tree' path' of
                                                       End n -> Just  (path', End $ n + a)
                                                       _ -> Nothing
                        right = case getNeighbourPath tree' R path of
                                    Nothing -> Nothing
                                    Just path' -> case lookupByPath tree' path' of
                                                       End n -> Just (path', End $ n + b)
                                                       _ -> Nothing

findSplit' :: [Path] -> ShellFishNum Int -> [Maybe [Path]]
findSplit' path (Number n m) = left ++ right
      where left = findSplit' (L:path) n
            right = findSplit' (R:path) m
findSplit' path (End n)
      | 10 <= n = [Just path]
      | otherwise = [Nothing]

findSplit :: ShellFishNum Int -> Maybe [Path]
findSplit n
      | null result = Nothing
      | otherwise = Just . minimum $ result
      where result = map reverse . catMaybes . findSplit' [] $ n

splits :: ShellFishNum Int -> ShellFishNum Int
splits tree
      | isNothing path = tree
      | otherwise = insertByPath tree path' new
      where path = findSplit tree
            path' = fromJust path
            End n = lookupByPath tree path'
            n' = n `div` 2
            new = if even n
                  then Number (End n') (End n')
                  else Number (End n') (End (n' + 1))

repExplode :: ShellFishNum Int -> ShellFishNum Int
repExplode prev
      | next == prev = next
      | otherwise = repExplode next
      where next = explode prev

simplify :: ShellFishNum Int -> ShellFishNum Int
simplify prev
      | next == prev = next
      | otherwise = simplify next
      where next = splits . repExplode $ prev

addition :: ShellFishNum Int -> ShellFishNum Int -> ShellFishNum Int
addition a b = simplify $ Number a b

summation :: [ShellFishNum Int] -> ShellFishNum Int
summation = foldl1 addition

magnitude :: ShellFishNum Int -> Int
magnitude (Number n m) = 3 * magnitude n + 2 * magnitude m
magnitude (End n) = n

solve1 :: String -> Int
solve1 = magnitude . summation . parse

solve2 :: String -> Int
solve2 str = maximum $ [(magnitude . uncurry addition) (x, y) | x <- nums, y <- nums, x /= y]
      where nums = parse str

main :: IO ()
main = do
      input <- getContents
      print . solve1 $ input
      print . solve2 $ input