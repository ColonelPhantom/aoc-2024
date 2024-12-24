import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (singleton, intersperse, intercalate)

readConnection :: String -> (String, String)
readConnection xs = let [a, b] = splitOn "-" xs in (a,b)

toMap :: [(String, String)] -> M.Map String (S.Set String)
toMap = M.fromListWith (<>) . concatMap (\(a,b) -> [(a, S.singleton b), (b, S.singleton a)])

potentialChiefs :: M.Map String (S.Set String) -> [String]
potentialChiefs = filter ((== 't') . head) . M.keys

findTrio :: M.Map String (S.Set String) -> String -> S.Set (S.Set String)
findTrio m c = S.fromList $ concatMap candidates (m M.! c) where
    self = m M.! c
    candidates neighA = map (`S.insert` S.fromList [c, neighA]) $ filter (`S.member` self) (S.toList $ m M.! neighA)

expandGroup :: M.Map String (S.Set String) -> S.Set String -> S.Set (S.Set String)
expandGroup m s = S.fromList $ map (`S.insert` s) $ filter accepted $ M.keys m where
    accepted potentialNeigh = all (S.member potentialNeigh . (m M.!)) (S.toList s)

expandSet :: M.Map String (S.Set String) -> S.Set (S.Set String) -> S.Set (S.Set String)
expandSet m = S.unions . S.map (expandGroup m)

main = do
    input <- map readConnection . lines <$> getContents
    let neighs = toMap input

    putStr "part 1: "; print $ S.size $ S.unions $ map (findTrio neighs) $ potentialChiefs neighs

    let singletons = S.fromList $ map S.singleton $ M.keys neighs
    let sss = takeWhile (not . S.null) $ iterate (expandSet neighs) singletons
    mapM_ (\s -> print (S.size (S.findMin s), S.size s)) sss
    let strs = S.toAscList $ S.findMin $ last sss
    putStr "part 2: "; putStrLn $ intercalate "," strs