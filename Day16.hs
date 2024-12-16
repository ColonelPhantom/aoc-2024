import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bifunctor
import Debug.Trace (traceShow)

data Tile = Empty | Wall | Start | End deriving (Show, Eq)

parseBoard :: [String] -> M.Map Coord Tile
parseBoard xs = M.fromList elems where
    elems = concat (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = ((x,y), readTile char)

    readTile '#' = Wall
    readTile '.' = Empty
    readTile 'S' = Start
    readTile 'E' = End
    readTile _ = error "invalid tile"

data Dir = N | S | E | W deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Neighs a = a -> [(a, Int)]

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

neighs :: M.Map Coord Tile -> Neighs (Coord, Dir)
neighs m (c@(x,y), N) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x,y-1), N), 1), ((c, E), 1000), ((c, W), 1000)]
neighs m (c@(x,y), S) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x,y+1), S), 1), ((c, E), 1000), ((c, W), 1000)]
neighs m (c@(x,y), E) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x+1,y), E), 1), ((c, N), 1000), ((c, S), 1000)]
neighs m (c@(x,y), W) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x-1,y), W), 1), ((c, N), 1000), ((c, S), 1000)]

find :: M.Map Coord Tile -> Tile -> Coord
find m t = fst $ head $ filter ((== t) . snd) $ M.toList m

bfs' :: (Ord k, Show k) => Neighs k -> [(k, Int, S.Set k)] -> M.Map k (Int, S.Set k) -> M.Map k (Int, S.Set k)
bfs' f [] visited = visited
bfs' f ((q, dist, best):qs) visited = if new
    then bfs' f queue' visited'
    else bfs' f qs visited
    where
        (oldDist, oldBest) = visited M.! q
        new = M.notMember q visited || dist < oldDist || (dist == oldDist && not (all (`S.member` oldBest) best))
        newBest = S.insert q best
        neighs = map (\(c, d) -> (c, dist+d, newBest)) (f q)
        visited' = M.insertWith merge q (dist, newBest) visited
        merge (d', s') (d, s) = case compare d d' of
            LT -> (d, s)
            EQ -> (d, S.union s s')
            GT -> (d', s')
        queue' = qs ++ neighs


main :: IO ()
main = do
    board <- parseBoard . lines <$> getContents
    let start = find board Start
    let end = find board End

    let result = bfs' (neighs board) [((start, E), 0, S.empty)] M.empty
    let best = minimum $ map (fst . snd) $ filter ((== end) . fst . fst) $ M.toList result
    putStr "part 1: "; print best

    let sets = map (snd . snd) $ filter ((== best) . fst . snd) $ filter ((== end) . fst . fst) $ M.toList result
    let set = S.fromList $ map fst $ concatMap S.toList sets
    putStr "part 2: "; print (S.size set); print set
