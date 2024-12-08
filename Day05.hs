import Data.List.Split (splitOn)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

readRule :: String -> (Int, Int)
readRule r = let [a,b] = splitOn "|" r in (read a, read b)

readPrint :: String -> [Int]
readPrint = map read . splitOn ","

getMiddle :: [Int] -> Int
getMiddle xs = xs !! (length xs `div` 2)

inOrder :: M.Map Int [Int] -> [Int] -> Bool
inOrder m [] = True
inOrder m (x:xs) = not (any (`elem` xs) smallers) && inOrder m xs where
    smallers = fromMaybe [] (M.lookup x m)

compareBy :: M.Map Int [Int] -> Int -> Int -> Ordering
compareBy m a b
    | a `elem` belowB = LT
    | b `elem` belowA = GT
    | otherwise = EQ
    where
        belowA = fromMaybe [] $ M.lookup a m
        belowB = fromMaybe [] $ M.lookup b m

main = do
    (rules, prints) <- (\[r,p] -> (map readRule r, map readPrint p)) . splitOn [""] . lines <$> getContents
    let rulemap = M.fromListWith (++) (map (\(a,b) -> (b,[a])) rules)
    putStr "part 1: " >> print (sum $ map getMiddle $ filter (inOrder rulemap) prints)
    putStr "part 2: " >> print (sum $ map (getMiddle . sortBy (compareBy rulemap)) $ filter (not . inOrder rulemap) prints)
