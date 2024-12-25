import Data.Bits (xor)
import qualified Data.Map as M
import Data.Tuple (swap)
import Control.Parallel (par)
import Control.Parallel.Strategies

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

unionsMergeWith :: (Ord k, NFData k, NFData a) => (a -> a -> a) -> [M.Map k a] -> M.Map k a
unionsMergeWith f ms = case unionStep f ms `using` parList rdeepseq of 
    [] -> M.empty
    [m] -> m
    ms' -> unionsMergeWith f ms'
    where
    unionStep f [] = []
    unionStep f [m] = [m]
    unionStep f (m:n:ms) = M.unionWith f m n : unionStep f ms


main = do
    input <- map read . lines <$> getContents
    _ <- input `usingIO` rdeepseq
    putStr "part 1: "; print $ sum (map part1 input `using` parList rseq)

    let maps = map comboMap input `using` parList rseq
    let map1 = unionsMergeWith (+) maps
    let biggest = maximum $ map swap $ M.toList map1
    
    putStr "part 2: "; print biggest