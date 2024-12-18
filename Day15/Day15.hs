{-# LANGUAGE LambdaCase #-}
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
data Tile = Empty | Box | Wall | Robot deriving (Show, Eq)
data Move = U | D | L | R deriving (Show, Eq)

readTile :: Char -> Tile
readTile '#' = Wall
readTile '.' = Empty
readTile 'O' = Box
readTile '@' = Robot
readTile _ = error "invalid tile"

parseBoard :: [String] -> M.Map Coord Tile
parseBoard xs = M.fromList elems where
    elems = concat (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = ((x,y), readTile char)

showBoard :: M.Map Coord Tile -> [String]
showBoard m = map showLine lines where
    line y = filter ((== y) . snd . fst) $ M.toAscList m
    lines = takeWhile (not . null) $ map line [0..]
    showLine = map (showTile . snd) . sortOn fst

    showTile Wall = '#'
    showTile Empty = '.'
    showTile Box = 'O'
    showTile Robot = '@'

readMove :: Char -> Move
readMove '<' = L
readMove '>' = R
readMove '^' = U
readMove 'v' = D
readMove _ = error "invalid move"

step :: Move -> Coord -> Coord
step U (x,y) = (x,y-1)
step D (x,y) = (x,y+1)
step L (x,y) = (x-1,y)
step R (x,y) = (x+1,y)

move :: Move -> Coord -> M.Map Coord Tile -> Maybe (M.Map Coord Tile)
move dir c@(x,y) m = let next = step dir c in case m M.! next of
    Wall -> Nothing
    Empty -> Just $ M.insert next (m M.! c) $ M.insert c Empty m
    Box -> M.insert next (m M.! c) . M.insert c Empty <$> move dir next m

findRobot :: M.Map Coord Tile -> Coord
findRobot = fst . head . filter ((== Robot) . snd) . M.toList

moveRobot :: Move -> (Coord, M.Map Coord Tile) -> (Coord, M.Map Coord Tile)
moveRobot dir (c,m) = maybe (c,m) (step dir c,) (move dir c m)

gps :: M.Map Coord Tile -> Int
gps = sum . map (gps1 . fst) . filter ((== Box) . snd) . M.toList where
    gps1 (x,y) = x + 100 * y

data Tile2 = Empty2 | BoxL | BoxR | Wall2 | Robot2 deriving (Show, Eq)

convertMap :: M.Map Coord Tile -> M.Map Coord Tile2
convertMap = M.fromList . concatMap (uncurry go) . M.toList where
    go :: Coord -> Tile -> [(Coord, Tile2)]
    go (x,y) Empty = [((2*x,y), Empty2), ((2*x+1, y), Empty2)]
    go (x,y) Box = [((2*x,y), BoxL), ((2*x+1, y), BoxR)]
    go (x,y) Wall = [((2*x,y), Wall2), ((2*x+1, y), Wall2)]
    go (x,y) Robot = [((2*x,y), Robot2), ((2*x+1, y), Empty2)]

findRobot2 :: M.Map Coord Tile2 -> Coord
findRobot2 = fst . head . filter ((== Robot2) . snd) . M.toList

move2 :: Move -> Coord -> M.Map Coord Tile2 -> Maybe (M.Map Coord Tile2)
move2 dir c@(x,y) m = let next@(x', y') = step dir c in case m M.! next of
    Wall2 -> Nothing
    Empty2 -> Just $ M.insert next (m M.! c) $ M.insert c Empty2 m
    -- BoxL -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
    -- BoxR -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
    BoxL -> case dir of
        L -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
        R -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
        _ -> M.insert next (m M.! c) . M.insert c Empty2 <$> moveMultiple dir next (x'+1,y') m
    BoxR -> case dir of
        L -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
        R -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
        _ -> M.insert next (m M.! c) . M.insert c Empty2 <$> moveMultiple dir next (x'-1,y') m
    where
        moveMultiple dir c1 c2 m = move2 dir c1 m >>= move2 dir c2
        -- moveOther dir c@(x,y) m = let next = step dir c in case m M.! next of
        --     Wall2 -> Nothing
        --     Empty2 -> Just $ M.insert next (m M.! c) $ M.insert c Empty2 m
        --     BoxL -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m
        --     BoxR -> M.insert next (m M.! c) . M.insert c Empty2 <$> move2 dir next m


moveRobot2 :: Move -> (Coord, M.Map Coord Tile2) -> (Coord, M.Map Coord Tile2)
moveRobot2 dir (c,m) = maybe (c,m) (step dir c,) (move2 dir c m)

showBoard2 :: M.Map Coord Tile2 -> [String]
showBoard2 m = map showLine lines where
    line y = filter ((== y) . snd . fst) $ M.toAscList m
    lines = takeWhile (not . null) $ map line [0..]
    showLine = map (showTile2 . snd) . sortOn fst

    showTile2 Wall2 = '#'
    showTile2 Empty2 = '.'
    showTile2 BoxL = '['
    showTile2 BoxR = ']'
    showTile2 Robot2 = '@'

gps2 :: M.Map Coord Tile2 -> Int
gps2 = sum . map (gps1 . fst) . filter ((== BoxL) . snd) . M.toList where
    gps1 (x,y) = x + 100 * y


main :: IO ()
main = do
    [board, moves] <- splitOn [""] . lines <$> getContents
    let b = parseBoard board
    let ms = map readMove $ concat moves
    let robot = findRobot b
    putStr "part 1: "
    print $ gps $ snd $ foldl (flip moveRobot) (findRobot b, b) ms

    let b2 = convertMap b

    putStr "part 2: "
    print $ gps2 $ snd $ foldl (flip moveRobot2) (findRobot2 b2, b2) ms

