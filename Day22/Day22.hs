import qualified Data.Array as A
import qualified Data.HashMap.Strict as HM
import Data.Bits (xor)
import Data.Tuple (swap)
import Control.Parallel.Strategies ( parList, rseq, using )

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

combos :: Int -> [((Int, Int, Int, Int), Int)]
combos secret = zip (toTuple <$> slices 4 (changes ps)) (drop 4 ps) where
    ps = take 2000 $ map (`mod` 10) $ iterate secrets secret

toTuple :: [Int] -> (Int, Int, Int, Int)
toTuple [a, b, c, d] = (a, b, c, d)

comboMap :: Int -> HM.HashMap (Int, Int, Int, Int) Int
comboMap = HM.fromListWith (\a b -> b) . combos

pricesArray :: [HM.HashMap (Int, Int, Int, Int) Int] -> A.Array (Int, Int, Int, Int) Int
pricesArray maps = A.accumArray (+) 0 ((-9, -9, -9, -9), (9, 9, 9, 9)) (concatMap HM.toList maps)

main :: IO ()
main = do
    input <- map read . lines <$> getContents

    putStr "part 1: "; print $ sum (map part1 input `using` parList rseq)

    let maps = map comboMap input `using` parList rseq
    let arr = pricesArray maps
    let (biggest, best) = maximum $ map swap $ A.assocs arr

    putStr "part 2: "; print (biggest, best)