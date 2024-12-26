import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (singleton, intercalate, partition, maximumBy, intersect)
import Data.Function (on)

-- Parsing
readConnection :: String -> (String, String)
readConnection xs = let [a, b] = splitOn "-" xs in (a,b)

toMap :: [(String, String)] -> M.Map String (S.Set String)
toMap = M.fromListWith (<>) . concatMap (\(a,b) -> [(a, S.singleton b), (b, S.singleton a)])

-- Part 1
potentialChiefs :: M.Map String (S.Set String) -> [String]
potentialChiefs = filter ((== 't') . head) . M.keys

findTrio :: M.Map String (S.Set String) -> String -> S.Set (S.Set String)
findTrio m c = S.fromList $ concatMap candidates (m M.! c) where
    self = m M.! c
    candidates neighA = map (`S.insert` S.fromList [c, neighA]) $ filter (`S.member` self) (S.toList $ m M.! neighA)

-- Part 2

-- from Wikipedia: Bron-Kerbosch algorithm
-- algorithm BronKerbosch1(R, P, X) is
--     if P and X are both empty then
--         report R as a maximal clique
--     for each vertex v in P do
--         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
--         P := P \ {v}
--         X := X ⋃ {v}

bronKerbosch :: Ord a => M.Map a (S.Set a) -> [S.Set a]
bronKerbosch m = go [] (M.keys m) [] where
    intersectWith a = filter (\x -> S.member x (m M.! a))
    go r [] [] = [S.fromList r]
    go r [] x = []
    go r (v:p) x = go (v:r) (intersectWith v p) (intersectWith v x) ++ go r p (v:x)

main :: IO ()
main = do
    input <- map readConnection . lines <$> getContents
    let neighs = toMap input

    putStr "part 1: "; print $ S.size $ S.unions $ map (findTrio neighs) $ potentialChiefs neighs
    -- mapM_ print $ bronKerbosch neighs
    putStr "part 2: "; putStrLn $ intercalate "," $ S.toAscList $ maximumBy (compare `on` S.size) $ bronKerbosch neighs