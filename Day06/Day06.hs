import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Prelude hiding (Left, Right)
import Data.List (unfoldr)

data Dir = Up | Down | Left | Right deriving (Eq, Ord, Show)

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

-- loops :: Ord a => S.Set a -> (a -> Maybe a) -> a -> Bool
-- loops s f x
--     | x `S.member` s = True
--     | otherwise = case f x of
--                        Just x' -> loops (S.insert x s) f x'
--                        Nothing -> False

loops :: Ord a => S.Set a -> (a -> Maybe a) -> a -> Bool
loops s f x = case f x of
                   Just y -> S.member y s || loops (S.insert y s) f y
                   Nothing -> False

main = do
    input <- lines <$> getContents
    let (sx,sy) = starting input
    let dims@(m,n) = (length (head input), length input)
    let obst = S.fromList (obstacles input)
    let step'' pos = (\(dir,x,y) -> ((x,y), (dir,x,y))) <$> step obst dims pos
    let positions = unfoldr step'' (Up,sx,sy)
    let posmap = S.fromList positions
    putStr "part 1: " >> print (S.size posmap)

    let newmaps = S.toList $ S.fromList $ [S.insert (x,y) obst | (x,y) <- S.toList posmap, (x,y) /= (sx,sy)]
    let loopmaps = filter (\o -> loops S.empty (step o dims) (Up,sx,sy)) newmaps
--     mapM_ print loopmaps
    putStr "part 2: " >> print (S.size (S.fromList loopmaps))
