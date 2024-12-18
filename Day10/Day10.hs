module Day10 where

import qualified Data.Map as M
import qualified Data.Set as S
type Coord = (Int, Int)

parse :: [String] -> M.Map Coord Int
parse xs = M.fromList elems where
    elems = concat (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = ((x,y), read [char])

score :: (Coord -> a) -> ([a] -> a) -> (a -> Int) -> M.Map Coord Int -> Int
score to combine from m = sum $ M.elems $ M.mapWithKey getScore m where
    scoremap = M.mapWithKey score1 m
    score1 (x,y) 9 = to (x,y)
    score1 (x,y) n = combine $ map (scoremap M.!)
                             $ filter (\c -> M.lookup c m == Just (n+1))
                               [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    getScore c n = if n == 0 then from $ scoremap M.! c else 0

main :: IO ()
main = do
    input <- parse . lines <$> getContents
    print $ score S.singleton S.unions S.size input
    print $ score (const 1) sum id input