import Prelude hiding (Left, Right)
import Data.Maybe (catMaybes)
import Data.List (unfoldr)
import qualified Data.Array as A
import qualified Data.Set as S

data Dir = Up | Down | Left | Right deriving (Eq, Ord, Show, A.Ix)

obstacles :: [String] -> [(Int, Int)]
obstacles xs = elems where
    elems = concatMap catMaybes (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = if char == '#' then Just (x,y) else Nothing

starting :: [String] -> (Int, Int)
starting xs = elems where
    [elems] = concatMap catMaybes (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = if char == '^' then Just (x,y) else Nothing

right Up = Right
right Down = Left
right Right = Down
right Left = Up

move (Up, x, y) = (x,y-1)
move (Down,x,y) = (x,y+1)
move (Left,x,y) = (x-1,y)
move (Right,x,y)= (x+1,y)

outOfBounds (m,n) (x,y) = x < 0 || x >= m || y < 0 || y >= n

step :: S.Set (Int, Int) -> (Int, Int) -> (Dir, Int, Int) -> Maybe (Dir, Int, Int)
step obst dims pos@(dir, x, y)
    | move pos `S.member` obst = step obst dims (right dir, x, y)
    | outOfBounds dims (move pos) = Nothing
    | otherwise = let (nx,ny) = move pos in Just (dir,nx,ny)

loops :: Ord a => S.Set a -> (a -> Maybe a) -> a -> Bool
loops s f x = case f x of
                   Just y -> S.member y s || loops (S.insert y s) f y
                   Nothing -> False


stepArray :: S.Set (Int, Int) -> (Int, Int) -> A.Array (Dir, Int, Int) (Maybe (Dir, Int, Int))
stepArray obst dims@(w,h) = A.array ((Up,0,0), (Right,w,h)) [((d,x,y), go d x y) | d <- [Up,Down,Left,Right], x <- [0..w-1], y <- [0..h-1]] where
    go dir x y
        | move (dir,x,y) `S.member` obst = Just (right dir, x, y)
        | outOfBounds dims (move (dir,x,y)) = Nothing
        | otherwise = let (nx, ny) = move (dir,x,y) in go dir nx ny

stepObst :: A.Array (Dir, Int, Int) (Maybe (Dir, Int, Int)) -> (Int, Int) -> (Dir, Int, Int) -> Maybe (Dir, Int, Int)
stepObst arr obst@(ox,oy) pos@(dir, x, y) = case dir of
        Up    -> if x == ox && y > oy && not blocked then Just (right dir, x, oy+1) else old
        Down  -> if x == ox && y < oy && not blocked then Just (right dir, x, oy-1) else old
        Left  -> if y == oy && x > ox && not blocked then Just (right dir, ox+1, y) else old
        Right -> if y == oy && x < ox && not blocked then Just (right dir, ox-1, y) else old
    where
    old = arr A.! pos
    blocked = (onLine dir <$> old) == Just True
    onLine Up    (_, x', y') = x == x' && ox == x' && y' <= y && y' > oy
    onLine Down  (_, x', y') = x == x' && ox == x' && y' >= y && y' < oy
    onLine Left  (_, x', y') = y == y' && oy == y' && x' <= x && x' > ox
    onLine Right (_, x', y') = y == y' && oy == y' && x' >= x && x' < ox

main :: IO ()
main = do
    input <- lines <$> getContents
    let (sx,sy) = starting input
    let dims@(m,n) = (length (head input), length input)
    let obst = S.fromList (obstacles input)

    let unfoldStep pos = (\(dir,x,y) -> ((x,y), (dir,x,y))) <$> step obst dims pos
    let positions = unfoldr unfoldStep (Up,sx,sy)
    let posmap = S.fromList positions
    putStr "part 1: "; print $ S.size posmap

    let arr = stepArray obst dims
    let newmaps = S.toList $ S.delete (sx, sy) posmap
    let loopmaps = filter (\o -> loops S.empty (stepObst arr o) (Up,sx,sy)) newmaps
    putStr "part 2: "; print $ length loopmaps
