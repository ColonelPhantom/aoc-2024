import Data.Maybe (catMaybes)
import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A

type Coord = (Int, Int)
data Tile = Empty | Wall | Start | End deriving (Show, Eq)
type Neighs a = a -> [a]

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

path :: M.Map Coord Tile -> [Coord]
path m = unfoldr step (findStart m, S.empty) where
    step (c,visited) = case filter (`S.notMember` visited) (neighs m c) of
        [] -> if S.member c visited then Nothing else Just (c, (c, S.insert c visited))
        [x] -> Just (c, (x, S.insert c visited))
        xs -> error $ "more than one path: " ++ show xs

neighs :: M.Map Coord Tile -> Neighs Coord
neighs m (x,y) = filter isValid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] where
    isValid c = M.member c m && m M.! c /= Wall

{-# INLINE manhattan #-}
manhattan :: Coord -> Coord -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

pairs :: [a] -> [(a, [a])]
pairs [] = []
pairs (x:xs) = (x,xs) : pairs xs

cheatable :: [Coord] -> Int -> [(Int, Coord, Coord)]
cheatable p len = maps where
    maps = [ (m,(x1,y1),(x2,y2))
           | ((x1, y1), startTime) <- zip p [0..]          -- the cheat starts at (x,y) at time startTime
           , x2 <- [max 0 (x1 - len) .. min mx (x1 + len)] -- we find all reachable x2
           , y2 <- [max 0 (y1 - len) .. min my (y1 + len)] -- and y2 values
           , let m = manhattan (x1,y1) (x2,y2)             -- and calculate the actual (Manhattan) distance
           , m <= len                                      -- and check if it's less than the desired length
           , let endTime = a A.! (x2, y2)                  -- furthermore, we check the 'time' of the target square
           , (endTime - startTime) - m >= 100              -- and check it the cheat saves enough time
           ]
    a = A.accumArray (\_ e -> e) (-1) ((0,0), (mx, my)) $ zip p [0..]
    mx = maximum $ map fst p
    my = maximum $ map snd p

solve :: [Coord] -> Int -> Int
solve p len = length (cheatable p len)

main = do
    board <- parseBoard . lines <$> getContents
    let p = path board
    putStr "part 1: "; print $ solve p 2
    putStr "part 2: "; print $ solve p 20