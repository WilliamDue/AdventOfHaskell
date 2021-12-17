import Data.List.Split (splitOn)
import qualified Data.List as L

type Vel = (Int, Int)
type Pos = (Int, Int)
type Target = (Pos, Pos)

parse :: String -> Target
parse = toTuple
        . map (splitOn ".." . last . splitOn "=")
        . splitOn ", "
        . drop 2
        . dropWhile (/=':')
      where toTuple [[minX, maxX], [minY, maxY]] = ((read minX, read maxX), (read minY, read maxY))
            toTuple _ = error "Invalid syntax."

moveProbe' :: (Pos, Vel) -> (Pos, Vel)
moveProbe' ((x, y), (x', y')) = ((x + x', y + y'), (x' - signum x', y' - 1))

moveProbe :: Pos -> Vel -> [(Pos, Vel)]
moveProbe n n' = iterate moveProbe' (n, n')

simulateProbe :: Target -> Pos -> Vel -> [(Pos, Vel)]
simulateProbe target n n' = if L.null result then temp else result
      where temp = takeWhile ((minY<=) . snd . fst) 
                   $ moveProbe n n'
            result = reverse 
                     . dropWhile (not . isAHit target . fst)
                     . reverse 
                     $ temp
            ((minX, maxX), (minY, maxY)) = target

isAHit :: Target -> Pos -> Bool
isAHit target (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY
      where ((minX, maxX), (minY, maxY)) = target

didtrajectoryHit :: Target -> [(Pos, Vel)] -> Bool
didtrajectoryHit target =  isAHit target . fst . last

initVel :: Pos -> Target -> Vel
initVel pos target = findMaxVel pos target vels
      where vels = [(n, n) | n <- [0..100]]

newVels :: (Pos, Pos) -> Vel -> [Vel]
newVels ((minX, maxX), (minY, maxY)) (x, y) = newVels'
      where newVels' = [ (x + x', y + y') | x' <- [minX .. maxX], y' <- [minY .. maxY]]

findMaxVel :: Pos -> Target -> [Vel] -> Vel
findMaxVel pos target = fst
                        . last
                        . L.sortOn (L.maximum . map (snd . fst) . snd)
                        . filter (didtrajectoryHit target . snd)
                        . map (\vel -> (vel, simulateProbe target pos vel))

improveVels :: (Pos, Pos) -> Pos -> Target -> Vel -> Vel
improveVels range pos target = findMaxVel pos target . newVels range

iterImproveVels :: (Pos, Pos) -> Pos -> Target -> [Vel]
iterImproveVels range pos target = iterate (improveVels range pos target) $ initVel pos target

solve1 :: Int -> (Pos, Pos) -> Pos -> Target -> Int
solve1 n range pos target = maximum
                            . map (snd . fst)
                            . simulateProbe target pos
                            . last
                            . take n
                            $ iterImproveVels range pos target

-- solve2 :: (Pos, Pos) -> Pos -> Target -> Int
solve2 range pos target = newVels'
      where newVels' = filter (didtrajectoryHit target . simulateProbe target pos)
                       . newVels range
                       $ initVel pos target

temp :: [Vel]
temp = [(23,-10), (25,-9), ( 27,-5), ( 29,-6), ( 22,-6), ( 21,-7), ( 9,0), ( 27,-7), ( 24,-5), (25,-7), ( 26,-6), ( 25,-5), ( 6,8), ( 11,-2), ( 20,-5), ( 29,-10), (6,3), ( 28,-7), (8,0), ( 30,-6), ( 29,-8), ( 20,-10), (6,7), ( 6,4), ( 6,1), ( 14,-4), ( 21,-6), (26,-10), (7,-1), (7,7), ( 8,-1), (21,-9), ( 6,2), ( 20,-7), ( 30,-10), (14,-3), (20,-8), ( 13,-2), ( 7,3), ( 28,-8), ( 29,-9), ( 15,-3), ( 22,-5), ( 26,-8), ( 25,-8), (25,-6), ( 15,-4), ( 9,-2), (15,-2), ( 12,-2), ( 28,-9), ( 12,-3), ( 24,-6), ( 23,-7), (25,-10), (7,8), ( 11,-3), ( 26,-7), ( 7,1), ( 23,-9), ( 6,0), ( 22,-10), (27,-6), (8,1), ( 22,-8), ( 13,-4), ( 7,6), ( 28,-6), ( 11,-4), ( 12,-4), ( 26,-9), ( 7,4), (24,-10), (23,-8), ( 30,-8), ( 7,0), ( 9,-1), (10,-1), ( 26,-5), ( 22,-9), ( 6,5), (7,5), ( 23,-6), ( 28,-10), (10,-2), ( 11,-1), ( 20,-9), ( 14,-2), ( 29,-7), ( 13,-3), (23,-5), ( 24,-8), ( 27,-9), ( 30,-7), ( 28,-5), ( 21,-10), (7,9), ( 6,6), ( 21,-5), (27,-10), (7,2), ( 30,-9), ( 21,-8), ( 22,-7), ( 24,-9), ( 20,-6), ( 6,9), ( 29,-5), (8,-2), (27,-8), ( 30,-5), ( 24,-7)]


main :: IO ()
main = do
      input <- getContents
      let target = parse input
      let range1 = ((-50, 50), (-50, 50))
      let range2 = ((-50, 500), (-500, 500))
      print . solve1 100 range1 (0, 0) $ target
      print . length $ solve2 range2 (0, 0) target