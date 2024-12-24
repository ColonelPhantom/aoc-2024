import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Prelude hiding (Left, Right)
import Data.List (unfoldr)
import qualified Data.Bifunctor as Bf
import Control.Parallel.Strategies

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

{-# INLINE right #-}
right Up = Right
right Down = Left
right Right = Down
right Left = Up

{-# INLINE move #-}
move (Up, x, y) = (x,y-1)
move (Down,x,y) = (x,y+1)
move (Left,x,y) = (x-1,y)
move (Right,x,y)= (x+1,y)

{-# INLINE outOfBounds #-}
outOfBounds (m,n) (x,y) = x < 0 || x >= m || y < 0 || y >= n

step :: S.Set (Int, Int) -> (Int, Int) -> (Dir, Int, Int) -> Maybe ((Dir, Int, Int), Bool)
step obst dims pos@(dir, x, y)
    | move pos `S.member` obst = Bf.second (const True) <$> step obst dims (right dir, x, y)
    | outOfBounds dims (move pos) = Nothing
    | otherwise = let (nx,ny) = move pos in Just ((dir,nx,ny), False)

loops :: Ord a => S.Set a -> (a -> Maybe (a, Bool)) -> a -> Bool
loops s f x = case f x of
                   Just (y, significant) -> if significant then S.member y s || loops (S.insert y s) f y else loops s f y
                   Nothing -> False

main = do
    input <- lines <$> getContents
    let (sx,sy) = starting input
    let dims@(m,n) = (length (head input), length input)
    let obst = S.fromList (obstacles input)
    let step'' pos = (\(dir,x,y) -> ((x,y), (dir,x,y))) . fst <$> step obst dims pos
    let positions = unfoldr step'' (Up,sx,sy)
    let posmap = S.fromList positions
    putStr "part 1: " >> print (S.size posmap)

    let newmaps = S.toList $ S.fromList $ [S.insert (x,y) obst | (x,y) <- S.toList posmap, (x,y) /= (sx,sy)]
    let bools = map (\o -> loops S.empty (step o dims) (Up,sx,sy)) newmaps `using` parList rseq
    let loopmaps = map fst $ filter snd $ zip newmaps bools
    putStr "part 2: " >> print (S.size (S.fromList loopmaps))
