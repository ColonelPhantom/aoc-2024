module Day08 where


import Data.List.Split
import Data.Maybe (catMaybes)
import Data.Char (isAlpha, isDigit)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bifunctor
import Debug.Trace (traceShowId)

type Coord = (Int, Int)

findAntennas :: [String] -> [(Char, Coord)]
findAntennas xs = elems where
    elems = concatMap catMaybes (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = if isDigit char || isAlpha char then Just (char, (x,y)) else Nothing

findAntis :: M.Map Char [Coord] -> (Char, Coord) -> S.Set Coord
findAntis m (c, (x,y)) = S.fromList antis where
    neighbors = filter (/= (x,y)) (m M.! c)
    diffs = map (\(x',y') -> (x'-x,y'-y)) neighbors
    antis = map (Data.Bifunctor.bimap (x -) (y -)) diffs

findAntis2 :: (Int, Int) -> M.Map Char [Coord] -> (Char, Coord) -> S.Set Coord
findAntis2 (w,h) m (c, (x,y)) = S.fromList antis where
    neighbors = filter (/= (x,y)) (m M.! c)
    diffs = map (\(x',y') -> (x'-x,y'-y)) neighbors
    inRange (x,y) = x >= 0 && x < w && y >= 0 && y < h
    antis = concatMap (\(dx, dy) -> takeWhile inRange $ iterate (\(ax,ay) -> (ax-dx,ay-dy)) (x,y)) diffs

main = do
    input <- lines <$> getContents
    let (w,h) = (length (head input), length input)
    let antennas = M.fromListWith (++) $ map (\(k,v) -> (k, [v])) $ findAntennas input
    let antis = S.unions $ map (findAntis antennas) (findAntennas input)
    let inRange (x,y) = x >= 0 && x < w && y >= 0 && y < h
    print $ length $ filter inRange $ S.toList antis

    let antis2 = S.unions $ map (findAntis2 (w,h) antennas) (findAntennas input)
    print antis2
    print $ length $ filter inRange $ S.toList antis2