safe :: [Int] -> Bool
safe xs = (and (zipWith (>) xs (tail xs)) || and (zipWith (<) xs (tail xs)))
        && and (zipWith (\x y -> abs (x-y) <= 3) xs (tail xs))

dampener :: [Int] -> [[Int]]
dampener [] = []
dampener (x:xs) = xs : map (x:) (dampener xs)

main :: IO ()
main = do
    input <- map (map read . words) . lines <$> getContents
    putStr "Part 1: " >> print (length (filter safe input))
    putStr "Part 2: " >> print (length (filter (any safe . dampener) input))
