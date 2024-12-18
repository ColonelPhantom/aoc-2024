import Data.List (tails, transpose)

-- Gets all chunks of length n: zips 3 "abcd" = ["abc", "bcd"]
zips :: Int -> [a] -> [[a]]
zips n xs
    | length xs >= n = take n xs : zips n (tail xs)
    | otherwise = []

-- Get all blocks of n*n in similar fashion to zips
blocks :: Int -> [[a]] -> [[[a]]]
blocks n xss = concatMap (zips n) $ transpose $ map (zips n) xss

-- Gets the diagonal starting at the top left
diagonal1 :: [[a]] -> [a]
diagonal1 [] = []
diagonal1 ([]:_) = []
diagonal1 ((x:xs):ys) = x : diagonal1 (map tail ys)

-- Gets all diagonals that go down and to the right
diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals ([]:_) = []
diagonals xss = [origin] ++ verts ++ horts where
    -- We have the diagonal that starts at the top left,
    origin = diagonal1 xss
    -- all other diagonals that start in the first column
    verts = map diagonal1 (tails (tail xss))
    -- and all other diagonals that start in the first row
    horts = map diagonal1 (transpose $ tails $ tail $ transpose xss)

-- Gets all lines in all directions including diagonals
getAll :: [[a]] -> [[a]]
getAll xss = concat [right, left, down, up, southwest, southeast, northwest, northeast] where
    right = xss
    left = map reverse xss
    down = transpose xss
    up = map reverse $ transpose xss
    southwest = diagonals xss
    southeast = diagonals (map reverse xss)
    northwest = diagonals (reverse xss)
    northeast = diagonals (map reverse (reverse xss))

-- Check if a 3x3 blocks consists of two crossed MAS-es
isXmas :: [[Char]] -> Bool
isXmas xss = (diagonal1 xss == "MAS" || diagonal1 xss == "SAM")
          && (diagonal1 yss == "MAS" || diagonal1 yss == "SAM")
    where yss = reverse xss

main = do
    input <- lines <$> getContents
    putStr "part 1: " >> print (length $ concatMap (filter (== "XMAS") . zips 4) (getAll input))
    putStr "part 2: " >> print (length $ filter isXmas $  blocks 3 input)
