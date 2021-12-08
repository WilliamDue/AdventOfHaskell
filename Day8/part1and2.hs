import Data.List.Split ( splitOn, )
import qualified Data.Set as Set


parse :: String -> [([String], [String])]
parse = map (toTuple . map words . splitOn " | ") . lines
      where toTuple [a, b] = (a, b)
            toTuple _ = error "wrong length"

countNaive :: ([String], [String]) -> Int
countNaive (_, b) =  length . filter aux $ map length b
      where aux :: Int -> Bool
            aux n = n == 4 || n == 2 || n == 3 || n == 7

parseSet :: ([String], [String]) -> ([Set.Set Char], [Set.Set Char])
parseSet (a, b) = (toSet a, toSet b)
      where toSet = map Set.fromList

translate :: [Set.Set Char] -> [(Set.Set Char, Int)]
translate a = [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6),
                    (seven, 7), (eight, 8), (nine, 9)]
      where one = head $ filter ((==2) . Set.size) a
            seven = head $ filter ((==3) . Set.size) a
            eight = head $ filter ((==7) . Set.size) a
            four = head $ filter ((==4) . Set.size) a
            sixL = filter ((==6) . Set.size) a
            fiveL = filter ((==5) . Set.size) a
            nine = head . filter (Set.isSubsetOf four) $ filter (Set.isSubsetOf one) sixL
            zero = head . filter (not . Set.isSubsetOf four) $ filter (Set.isSubsetOf one) sixL
            six = head $ filter (not . Set.isSubsetOf one) sixL
            three = head . filter (Set.isSubsetOf seven) $ filter (`Set.isSubsetOf` nine) fiveL
            five = head . filter (not . Set.isSubsetOf seven) $ filter (`Set.isSubsetOf` nine) fiveL
            two = head $ filter (not . flip Set.isSubsetOf nine) fiveL

comptueNumber :: ([String], [String]) -> Int
comptueNumber n = sum . zipWith (*) [10^n | n <- [0..]] . reverse $ map (snd . findNum) b
      where (a, b) = parseSet n
            trans = translate a
            findNum m = head $ filter ((==m) . fst) trans 

main :: IO ()
main = do
      content <- getContents
      print . sum . map countNaive  $ parse content
      print . sum . map comptueNumber $ parse content