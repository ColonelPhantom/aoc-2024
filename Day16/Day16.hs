import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bifunctor as Bf
import qualified Data.List.NonEmpty as NE

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

neighs :: M.Map Coord Tile -> Neighs (Coord, Dir)
neighs m (c@(x,y), N) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x,y-1), N), 1), ((c, E), 1000), ((c, W), 1000)]
neighs m (c@(x,y), S) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x,y+1), S), 1), ((c, E), 1000), ((c, W), 1000)]
neighs m (c@(x,y), E) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x+1,y), E), 1), ((c, N), 1000), ((c, S), 1000)]
neighs m (c@(x,y), W) = filter (\((c', _), _) -> m M.! c /= Wall) [(((x-1,y), W), 1), ((c, N), 1000), ((c, S), 1000)]

find :: M.Map Coord Tile -> Tile -> Coord
find m t = fst $ head $ filter ((== t) . snd) $ M.toList m

data Queue k = Queue
    { cost :: M.Map Int (NE.NonEmpty k)
    , from :: M.Map k (Int, S.Set k)
    }

singletonQueue :: Ord k => k -> Queue k
singletonQueue k = enqueue (k, 0, S.empty) (Queue M.empty M.empty)

enqueue :: Ord k => (k, Int, S.Set k) -> Queue k -> Queue k
enqueue (k, dist, set) q@(Queue cost from) = case M.lookup k from of
    Nothing -> Queue (M.insertWith (<>) dist (NE.singleton k) cost)        (M.insert k (dist, set) from)                 -- coord not in queue, just add it
    Just (origDist, origSet) -> case compare dist origDist of
        LT ->  Queue (M.update (NE.nonEmpty . NE.filter (/= k)) dist cost) (M.insert k (dist, set) from)                 -- coord in queue, but with a higher cost, override it
        EQ ->  Queue cost                                                  (M.adjust (Bf.second (`S.union` set)) k from) -- coord in queue, with same cost, merge it
        GT ->  q                                                                                                         -- coord in queue with lower cost, skip adding                                                                  

dequeue :: Ord k => Queue k -> Maybe ((k, Int, S.Set k), Queue k)
dequeue (Queue cost from)
    | M.null cost = Nothing
    | otherwise = Just ((cheapestCoord, lowestCost, set), Queue cost' from')
    where
        (lowestCost, cheapestCoord NE.:| cs) = M.findMin cost
        set = snd $ from M.! cheapestCoord
        cost' = case NE.nonEmpty cs of
            Nothing -> M.delete lowestCost cost
            Just cs' -> M.insert lowestCost cs' cost
        from' = M.delete cheapestCoord from

bfs' :: (Ord k, Show k) => Neighs k -> Queue k -> M.Map k (Int, S.Set k) -> M.Map k (Int, S.Set k)
bfs' f queue visited = case dequeue queue of
    Nothing -> visited
    Just ((q, dist, best), qs) -> if new
        then bfs' f queue' visited'
        else bfs' f qs visited
        where
            (oldDist, oldBest) = visited M.! q
            new = M.notMember q visited || dist < oldDist || (dist == oldDist && not (all (`S.member` oldBest) best))
            newBest = S.insert q best
            neighs = map (\(c, d) -> (c, dist+d, newBest)) (f q)
            visited' = M.insertWith merge q (dist, newBest) visited
            merge (d', s') (d, s) = case compare d d' of
                LT -> (d, s) -- never happens
                EQ -> (d, S.union s s')
                GT -> (d', s')
            queue' = foldr enqueue qs neighs


main :: IO ()
main = do
    board <- parseBoard . lines <$> getContents
    let start = find board Start
    let end = find board End

    let result = bfs' (neighs board) (singletonQueue (start, E)) M.empty
    let best = minimum $ map (fst . snd) $ filter ((== end) . fst . fst) $ M.toList result
    putStr "part 1: "; print best

    let sets = map (snd . snd) $ filter ((== best) . fst . snd) $ filter ((== end) . fst . fst) $ M.toList result
    let set = S.fromList $ map fst $ concatMap S.toList sets
    putStr "part 2: "; print (S.size set)
