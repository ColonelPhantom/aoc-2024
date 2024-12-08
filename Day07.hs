import Data.List (sort)
import Data.List.Split (splitOn)

readEq :: String -> (Int, [Int])
readEq eq = (read test, map read (words vals)) where
    [test, vals] = splitOn ":" eq

part1 :: Int -> [Int] -> [Int]
part1 test = foldl1 go . map (: []) where
    go acc [x] = filter (<= test) $ map (x+) acc ++ map (x*) acc

possible :: (Int -> [Int] -> [Int]) -> (Int, [Int]) -> Bool
possible f (test, vals) = test `elem` f test vals

part2 :: Int -> [Int] -> [Int]
part2 test = foldl1 go . map (:[]) where
    go acc [x] = filter (<= test) $ map (x+) acc ++ map (x*) acc ++ map (`cat` x) acc
    cat x y = y + (x * (10 ^ (1 + floor (logBase 10 $ fromIntegral y))))
--     cat x y = read (show x ++ show y)

main = do
    input <- map readEq . lines <$> getContents
    putStr "part 1: " >> print (sum $ map fst $ filter (possible part1) input)
    putStr "part 2: " >> print (sum $ map fst $ filter (\x -> possible part1 x || possible part2 x) input)
--     mapM_ print $ map (\(t,v) -> (t, part2 t v)) input
