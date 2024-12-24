import Data.Bits (xor)
import qualified Data.Map as M
import Data.Tuple (swap)

secrets :: Int -> Int
secrets = step (*2048) . step (`div` 32) . step (*64) where
    mix s v = s `xor` v
    prune s = s `mod` 16777216
    step f s = prune (mix s (f s))

part1 :: Int -> Int
part1 initial = iterate secrets initial !! 2000

-- Part 2
changes :: [Int] -> [Int]
changes xs = zipWith (-) (tail xs) xs

slices :: Int -> [Int] -> [[Int]]
slices n xs = let s = take n xs in if length s < n then [] else s : slices n (tail xs)

combos :: Int -> [([Int], Int)]
combos secret = zip (slices 4 (changes ps)) (drop 4 ps) where
    ps = take 2000 $ map (`mod` 10) $ iterate secrets secret

comboMap :: Int -> M.Map [Int] Int
comboMap = M.fromListWith (\a b -> b) . combos

main = do
    input <- map read . lines <$> getContents
    putStr "part 1: "; print $ sum $ map part1 input

    let maps = map comboMap input
    mapM_ (\(i,m) -> print (i , M.size m)) (zip [0..] maps)
    let map1 = M.unionsWith (+) maps
    let biggest = maximum $ map swap $ M.toList map1
    
    putStr "part 2: "; print biggest