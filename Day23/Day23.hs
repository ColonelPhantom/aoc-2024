{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (singleton, intersperse, intercalate)

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
data Group = Group { inside :: S.Set String, neighs :: S.Set String } deriving (Show, Eq, Ord)

initGroups :: M.Map String (S.Set String) -> S.Set Group
initGroups m = S.fromList $ map mkGroup $ M.keys m where
    all = S.fromList $ M.keys m
    mkGroup x = Group (S.singleton x) (S.delete x all)

expandGroup :: M.Map String (S.Set String) -> Group -> S.Set Group
expandGroup m (Group inside neighs) = S.fromList $ map join $ filter accepted $ S.toList neighs where
    accepted potentialNeigh = all (S.member potentialNeigh . (m M.!)) inside
    join newMember = Group (S.insert newMember inside) $ S.filter (\n -> S.member n (m M.! newMember) && n /= newMember) neighs

expandSet :: M.Map String (S.Set String) -> S.Set Group -> S.Set Group
expandSet m = S.unions . S.map (expandGroup m)

main :: IO ()
main = do
    input <- map readConnection . lines <$> getContents
    let neighs = toMap input

    putStr "part 1: "; print $ S.size $ S.unions $ map (findTrio neighs) $ potentialChiefs neighs

    let singletons = initGroups neighs
    let sss = takeWhile (not . S.null) $ iterate (expandSet neighs) singletons
    mapM_ (\s -> print (S.size (inside $ S.findMin s), S.size s)) sss
    let strs = S.toAscList $ inside $ S.findMin $ last sss
    putStr "part 2: "; putStrLn $ intercalate "," strs
