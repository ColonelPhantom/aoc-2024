import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Bifunctor
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import Data.List (inits)
import Debug.Trace (traceShowId, traceShow)

type Coord = (Int, Int)

parseCoord :: String -> Coord
parseCoord xs = let [x,y] = map read $ splitOn "," xs in (x, y)

safeGrid :: Int -> [Coord] -> S.Set Coord
safeGrid size unsafe = let u = S.fromList unsafe in S.fromList $ [(x,y) | x <- [0..size], y <- [0..size], (x,y) `S.notMember` u]

bfs :: (Ord k, Show k) => Neighs k -> [(k, Int)] -> M.Map k Int -> M.Map k Int
bfs f [] visited = visited
bfs f ((q, dist):qs) visited = if new
    then bfs f queue' visited'
    else bfs f qs visited
    where
        new = M.notMember q visited || dist < visited M.! q
        neighs = map (Data.Bifunctor.second (dist +)) (f q)
        visited' = M.insertWith min q dist visited
        queue' = qs ++ neighs

type Neighs a = a -> [(a, Int)]
neighs :: S.Set Coord -> Neighs Coord
neighs s (x,y) = map (,1) $ filter (`S.member` s) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

findPath :: Int -> [Coord] -> Maybe Int
findPath size unsafe = (size, size) `M.lookup` bfs (neighs $ safeGrid size unsafe) [((0,0), 0)] M.empty

part1 :: Int -> Int -> [Coord] -> Int
part1 size fallen unsafe = fromJust $ findPath size (take fallen unsafe)

part2 :: Int -> [Coord] -> Coord
part2 size unsafe = unsafe !! (binarySearchD 0 (length unsafe) safeN - 1) where
    safeN n = traceShow n $ isNothing $ findPath size (take n unsafe)

binarySearchD :: Int -> Int -> (Int -> Bool) -> Int
binarySearchD lo hi p
  | lo == hi = lo
  | p mid     = binarySearchD lo mid p
  | otherwise = binarySearchD (mid+1) hi p
  where
    mid = (lo + hi) `div` 2

main = do
    falling <- map parseCoord . lines <$> getContents
    putStr "part 1 (example): "; print $ part1 6 12 falling
    putStr "part 1: "; print $ part1 70 1024 falling
    putStr "part 2 (example): "; print $ part2 6 falling
    putStr "part 2: "; print $ part2 70 falling

