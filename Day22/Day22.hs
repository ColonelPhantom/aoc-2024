import Data.Bits (xor)
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import Data.Tuple (swap)
import Control.Parallel (par)
import Control.Parallel.Strategies
import System.Mem (performMajorGC)


secrets :: Int -> Int
secrets = step (*2048) . step (`div` 32) . step (*64) where
    mix s v = s `xor` v
    prune s = s `mod` 16777216
    step f s = prune (mix s (f s))

part1 :: Int -> Int
part1 initial = iterate secrets initial !! 2000

-- Part 2
{-# INLINE changes #-}
changes :: [Int] -> [Int]
changes xs = zipWith (-) (tail xs) xs

{-# INLINE slices #-}
slices :: Int -> [Int] -> [[Int]]
slices n xs = let s = take n xs in if length s < n then [] else s : slices n (tail xs)

combos :: Int -> [(Int, Int)]
combos secret = zip (toInt <$> slices 4 (changes ps)) (drop 4 ps) where
    ps = take 2000 $ map (`mod` 10) $ iterate secrets secret

toInt :: [Int] -> Int
toInt = foldl (\a b -> a * 19 + b + 9) 0

fromInt :: Int -> [Int]
fromInt 0 = []
fromInt x = fromInt (x `div` 19) ++ [x `mod` 19 - 9]

comboMap :: Int -> IM.IntMap Int
comboMap = IM.fromListWith (\a b -> b) . combos

unionsMergeWith :: (NFData a) => (a -> a -> a) -> [IM.IntMap a] -> IM.IntMap a
unionsMergeWith f ms = case unionStep f ms `using` parList rdeepseq of 
    [] -> IM.empty
    [m] -> m
    ms' -> unionsMergeWith f ms'
    where
    unionStep f [] = []
    unionStep f [m] = [m]
    unionStep f (m:n:ms) = IM.unionWith f m n : unionStep f ms

main :: IO ()
main = do
    input <- map read . lines <$> getContents
    _ <- input `usingIO` rdeepseq
    putStr "part 1: "; print $ sum (map part1 input `using` parList rseq)
    performMajorGC

    let maps = map comboMap input `using` parList rseq
    let map1 = unionsMergeWith (+) maps
    let (biggest, best) = maximum $ map swap $ IM.toList map1
    
    putStr "part 2: "; print (biggest, fromInt best)