import Data.Maybe (isJust, isNothing, fromMaybe)
-- import Debug.Trace (traceShowId, traceShow)
import Data.Sequence ( (<|), (|>), dropWhileR, fromList, Seq(..), breakl, (><) )
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

data DefragBlock = Free | Orig Int | Used Int deriving Eq

defrag :: Seq (DefragBlock, Int) -> Seq (DefragBlock, Int)
defrag Empty = Empty
defrag (disk :|> (Free, len)) = defrag disk |> (Free, len) -- remove trailing empty blocks
defrag (disk :|> (Used id, len)) = defrag disk |> (Used id, len) -- skip over used blocks
defrag (disk :|> (Orig id, len)) = case ({-# SCC "defrag/breakl" #-} breakl (\(c,l) -> c == Free && l >= len) disk) of
    (_, Empty) -> defrag disk |> (Used id, len) -- no free space, skip over
    (hd, (Free, free) :<| tl) -> case compare free len of
        LT -> error "boom"
        EQ -> defrag $ hd >< (Used id, len) <| merge (tl |> (Free, len))
        GT -> defrag $ hd >< (Used id, len) <| merge (merge ((Free, free-len) <| tl) |> (Free, len))
    where
    merge ((Free, l1) :<| (Free, l2) :<| xs) = (Free, l1+l2) <| xs
    merge (xs :|> (Free, l1) :|> (Free, l2)) = xs |> (Free, l1+l2)
    merge xs = xs

defragList :: [(Maybe Int, Int)] -> [(Maybe Int, Int)]
defragList = map fromBlock . toList . defrag . fromList . map toBlock where
    toBlock (Nothing, len) = (Free, len)
    toBlock (Just id, len) = (Orig id, len)
    fromBlock (Free, len) = (Nothing, len)
    fromBlock (Used id, len) = (Just id, len)
    fromBlock (Orig id, len) = (Just id, len)

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

flatten :: [(Maybe Int, Int)] -> [Int]
flatten [] = []
flatten ((Nothing, len):xs) = replicate len 0 ++ flatten xs
flatten ((Just id, len):xs) = replicate len id ++ flatten xs

main = do
    input <- getContents
    putStr "part 1: " >> print (checksum $ compactList $ parse 0 input)
    putStr "part 2: " >> print (checksum $ flatten $ defragList $ parseBlocks 0 input)