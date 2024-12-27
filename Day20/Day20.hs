import qualified Data.Map as M
import qualified Data.Bifunctor as BiF
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map.Merge.Lazy as MM
import Control.Parallel.Strategies

type Coord = (Int, Int)
data Tile = Empty | Wall | Start | End deriving (Show, Eq)
type Neighs a = a -> [(a, Int)]

parseBoard :: [String] -> M.Map Coord Tile
parseBoard xs = M.fromList elems where
    elems = concatMap catMaybes (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = ((x,y),) <$> readTile char

    readTile '#' = Just Wall
    readTile '.' = Just Empty
    readTile 'S' = Just Start
    readTile 'E' = Just End
    readTile _ = error "invalid tile"

findStart :: M.Map a Tile -> a
findStart board = let [(c,_)] = filter ((== Start) . snd) $ M.toList board in c
findEnd :: M.Map a Tile -> a
findEnd board = let [(c,_)] = filter ((== End) . snd) $ M.toList board in c

bfs :: (Ord k, Show k) => Neighs k -> [(k, Int)] -> M.Map k Int -> M.Map k Int
bfs f [] visited = visited
bfs f ((q, dist):qs) visited = if new
    then bfs f queue' visited'
    else bfs f qs visited
    where
        new = M.notMember q visited || dist < visited M.! q
        neighs = map (BiF.second (dist +)) (f q)
        visited' = M.insertWith min q dist visited
        queue' = qs ++ neighs

neighs :: M.Map Coord Tile -> Neighs Coord
neighs m (x,y) = map (,1) $ filter isValid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] where
    isValid c = M.member c m && m M.! c /= Wall

getCheats :: M.Map Coord Tile -> [(Coord, Coord)]
getCheats m = do
    start@(x,y) <- M.keys $ M.filter (/= Wall) m
    step1 <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    (finish, _) <- neighs m step1
    if M.member finish m && m M.! finish /= Wall
        then return (start, finish)
        else []

-- cheatable :: M.Map Coord Tile -> Int -> Coord -> S.Set Coord
-- cheatable m 0 (x,y) = if M.member (x,y) m then S.singleton (x,y) else S.empty
-- cheatable m n (x,y) = S.unions
--                     $ map (cheatable m (n-1)) [(x,y), (x-1, y), (x+1, y), (x, y-1), (x, y+1)]

cheatable :: M.Map Coord Tile -> Int -> [(Coord, M.Map Coord Int)]
cheatable m len = A.assocs final `using` parListChunk 16 rdeepseq where
    (minx, miny) = minimum $ M.keys m
    (maxx, maxy) = maximum $ M.keys m

    final = A.array ((minx, miny), (maxx, maxy)) [((x,y), getFinal (x,y)) | x <- [minx..maxx], y <- [miny..maxy]]

    getFinal :: Coord -> M.Map Coord Int
    getFinal (x,y) = M.fromListWith (\a b -> b) $ concatMap (\d -> map (,d) $ S.toList $ memo A.! (d,x,y)) [1..len]

    memo :: A.Array (Int, Int, Int) (S.Set Coord)
    memo = A.array ((0, minx, miny), (len, maxx, maxy)) [((n, x,y), go (x,y) n) | n <- [0..len], x <- [minx..maxx], y <- [miny..maxy]]

    go (x,y) n
        | M.notMember (x,y) m = S.empty
        | n <= 0 = if m M.! (x,y) /= Wall then S.singleton (x,y) else S.empty
        | otherwise = S.unions $ map go1 [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        where
            go1 (x', y') = memo `safeLookup` (n-1, x', y')
            safeLookup a (n', x,y) = if x < minx || x > maxx || y < miny || y > maxy then S.empty else a A.! (n', x,y)

getCheatsN :: Int -> M.Map Coord Tile -> [(Int, Coord, Coord)]
getCheatsN len m = do
    (from, tos) <- cheatable m len
    (to, dist) <- M.toList tos
    if M.member from m && M.member to m && m M.! from /= Wall && m M.! to /= Wall
        then return (dist, from, to)
        else []

solve :: M.Map Coord Tile -> Int -> Int
solve m len = length $ filter ((>=100) . cheatSavings) cheats where
    start = findStart m
    end = findEnd m
    forward = bfs (neighs m) [(start, 0)] M.empty
    backward = bfs (neighs m) [(end, 0)] M.empty
    normalCost = forward M.! end
    cheats = getCheatsN len m
    cheatCost (dist, from, to) = forward M.! from + dist + backward M.! to
    cheatSavings c = normalCost - cheatCost c

main = do
    board <- parseBoard . lines <$> getContents
    putStr "part 1: "; print $ solve board 2
    putStr "part 2: "; print $ solve board 20