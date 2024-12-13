import qualified Data.Map as M

blink :: [Int] -> [Int]
blink = concatMap go1

go1 :: Int -> [Int]
go1 n
    | n == 0 = [1]
    | even (length (show n)) = let (a,b) = splitAt (length (show n) `div` 2) $ show n in [read a, read b]
    | otherwise = [n * 2024]

blinkFast :: M.Map Int Int -> M.Map Int Int
blinkFast m = M.fromListWith (+) $ concatMap go $ M.toList m where
    go (k,v) = map (,v) (go1 k)

main = do
    input <- map read . words <$> getLine
    let inputmap = M.fromListWith (+) $ map (,1) input
    let values = iterate blinkFast inputmap
    mapM_ print $ take 75 $ zipWith (\i m -> (i, M.size m, sum m)) [0..] $ values
    putStr "part 1: " >> print (sum $ values !! 25)
    putStr "part 2: " >> print (sum $ values !! 75)
