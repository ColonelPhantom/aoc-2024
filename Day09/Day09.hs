import Data.Maybe (isJust, isNothing, fromMaybe)
-- import Debug.Trace (traceShowId, traceShow)
import Data.Sequence ( (<|), dropWhileR, fromList, Seq(..) )
import Data.Foldable (Foldable(toList))

compact :: Seq (Maybe Int) -> Seq Int
compact Empty = Empty
compact (Just x :<| xs) = x <| compact xs
compact (Nothing :<| xs) = case dropWhileR isNothing xs of
    Empty -> Empty
    (xs' :|> Just x) -> x <| compact xs'

compactList :: [Maybe Int] -> [Int]
compactList = toList . compact . fromList

parse :: Int -> [Char] -> [Maybe Int]
parse idx [] = []
parse idx [x] = parse idx [x,'0']
parse idx (x:y:xs) = replicate (read [x]) (Just idx) ++ replicate (read [y]) Nothing ++ parse (idx+1) xs

parseBlocks :: Int -> [Char] -> [(Maybe Int, Int)]
parseBlocks idx [] = []
parseBlocks idx [x] = [(Just idx, read [x])]
parseBlocks idx (x:y:xs) = (Just idx, read [x]) : (Nothing, read [y]) : parseBlocks (idx+1) xs

defrag :: [(Maybe Int, Int)] -> [(Maybe Int, Int)]
defrag blocks = foldr defrag1 blocks candidates where
    candidates = map (\(Just id, len) -> (id,len)) $ filter (\(x,y) -> isJust x) blocks

    defrag1 (id, len) ((Nothing, free):xs) = case compare free len of
        LT -> (Nothing, free) : defrag1 (id, len) xs
        EQ -> (Just id, len) : delete1 (id, len) xs
        GT -> (Just id, len) : (Nothing, free-len) : delete1 (id, len) xs
    defrag1 (id, len) disk@((Just id', l):xs) = if id == id' then disk else (Just id', l) : defrag1 (id, len) xs

    delete1 (id, len) [] = []
    delete1 (id, len) (x:xs) = if x == (Just id, len) then merge1 ((Nothing, len) : xs) else x : delete1 (id, len) xs

    merge1 ((Nothing,l) : (Nothing, l') : xs) = (Nothing, l+l') : xs
    merge1 xs = xs

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

flatten :: [(Maybe Int, Int)] -> [Int]
flatten [] = []
flatten ((Nothing, len):xs) = replicate len 0 ++ flatten xs
flatten ((Just id, len):xs) = replicate len id ++ flatten xs

main = do
    input <- getContents
    putStr "part 1: " >> print (checksum $ compactList $ parse 0 input)
    putStr "part 2: " >> print (checksum $ flatten $ defrag $ parseBlocks 0 input)
