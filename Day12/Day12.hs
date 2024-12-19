import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, mapMaybe)
import Prelude hiding (Left, Right)
import Data.List (delete)

type Coord = (Int, Int)

parse :: [String] -> M.Map Coord Char
parse xs = M.fromList elems where
    elems = concat (zipWith linep [0..] xs)
    linep y = zipWith (`entry` y) [0..]
    entry x y char = ((x,y), char)

findSet :: M.Map Coord Char -> Coord -> S.Set Coord
findSet m c = go [c] S.empty where
    color = m M.! c
    go [] s = s
    go (q:qs) s = go (filter (`S.notMember` s) (neighs q) ++ qs) (S.insert q s)
    neighs (x,y) = filter (\x -> M.member x m && m M.! x == color) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

findGroups :: M.Map Coord Char -> S.Set (S.Set Coord)
findGroups m = S.fromList $ go (M.keys m) [] S.empty where
    go [] ss total = ss
    go (q:qs) ss total = if S.member q total
        then go qs ss total
        else let self = findSet m q in go qs (self : ss) (S.union self total)

perimeter :: S.Set Coord -> Int
perimeter s = sum $ map untouching $ S.toList s where
    untouching (x,y) = length $ filter (`S.notMember` s) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

cost1 :: S.Set Coord -> Int
cost1 s = perimeter s * S.size s

data Side = Up | Down | Left | Right deriving (Eq, Show)
fences :: S.Set Coord -> [(Coord, Side)]
fences s = concatMap untouching $ S.toList s where
    untouching (x,y) = map (\(_,d) -> ((x,y),d)) $ filter ((`S.notMember` s) . fst)
                        [((x-1,y), Left), ((x+1,y), Right), ((x,y-1), Up), ((x,y+1), Down)]

sides :: [(Coord, Side)] -> Int
sides [] = 0
sides (((x,y), d):fs) = case d of
                            Up -> 1 + sides (deleteLeft (x,y) $ deleteRight (x,y) fs)
                            Down -> 1 + sides (deleteLeft (x,y) $ deleteRight (x,y) fs)
                            Left -> 1 + sides (deleteUp (x,y) $ deleteDown (x,y) fs)
                            Right -> 1 + sides (deleteUp (x,y) $ deleteDown (x,y) fs)
    where
        deleteUp (x,y) xs = if ((x,y-1), d) `elem` xs then deleteUp (x,y-1) (delete ((x,y-1), d) xs) else xs
        deleteDown (x,y) xs = if ((x,y+1), d) `elem` xs then deleteDown (x,y+1) (delete ((x,y+1), d) xs) else xs
        deleteLeft (x,y) xs = if ((x-1,y), d) `elem` xs then deleteLeft (x-1,y) (delete ((x-1,y), d) xs) else xs
        deleteRight (x,y) xs = if ((x+1,y), d) `elem` xs then deleteRight (x+1,y) (delete ((x+1,y), d) xs) else xs

cost2 :: S.Set Coord -> Int
cost2 s = sides (fences s) * S.size s

main :: IO ()
main = do
    input <- parse . lines <$> getContents
    let groups = findGroups input

    putStr "part 1: " >> print (sum $ map cost1 $ S.toList groups)
    putStr "part 2: " >> print (sum $ map cost2 $ S.toList groups)