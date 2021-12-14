{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.List.Split ( splitOn )
import Data.Bifunctor (bimap)

toTuple :: [b] -> (b, b)
toTuple [a, b] = (a, b)
toTuple n = error "input error"

type Operation = ((Char, Char), Char)
type PolyChains = M.Map (Char, Char) Integer
type PolyCount = M.Map Char Integer
type Poly = (PolyChains, PolyCount)

parse :: String -> ([Operation], Poly)
parse blob = (oper, (poly, polyCount))
      where (poly', oper') = toTuple $ splitOn "\n\n" blob
            oper = map (bimap toTuple head . toTuple . splitOn " -> ") $ lines oper'
            poly = foldl1 (M.unionWith (+))
                           . zipWith (curry (M.fromList
                           . (: []) . (,1))) poly'
                   $ tail poly'
            polyCount = foldl1 (M.unionWith (+)) $ map (M.fromList . (\n -> [(n,1)])) poly'

newPoly :: PolyChains -> Operation -> Poly
newPoly polyCount (oper, to) = (new, M.fromList [(to, count)])
      where count = M.findWithDefault 0 oper polyCount
            new = M.fromList [((fst oper, to), count), ((to, snd oper), count)]

applyOper :: [Operation] -> Poly -> Poly
applyOper opers (poly, count) = (newPoly', newCount)
      where polysWithCount = map (newPoly poly) opers
            newPoly' = foldl1 (M.unionWith (+)) $ map fst polysWithCount
            newCount = foldl (M.unionWith (+)) count $ map snd polysWithCount

solve :: Int -> [Operation] -> Poly -> Integer
solve n oper poly = max' - min'
      where poly' = last . take (n + 1) $ iterate (applyOper oper) poly
            min' = minimum $ snd poly'
            max' = maximum $ snd poly'

main :: IO ()
main = do
      input <- getContents
      let (oper, poly) = parse input
      print $ solve 10 oper poly
      print $ solve 40 oper poly