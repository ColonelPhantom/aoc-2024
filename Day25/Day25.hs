import Data.List.Split (splitOn)
import Data.List (transpose)

isKey :: [String] -> Bool
isKey xs = all (== '#') (last xs)

isLock :: [String] -> Bool
isLock xs = all (== '#') (head xs)

keyToList :: [String] -> [Int]
keyToList xs = map (length . takeWhile (=='#') . tail . reverse) (transpose xs)

lockToList :: [String] -> [Int]
lockToList xs = map (length . takeWhile (=='#')) $ transpose (tail xs)

fits :: [Int] -> [Int] -> Bool
fits xs ys = all (<6) (zipWith (+) xs ys)

main :: IO ()
main = do
    schematics <- splitOn [""] . lines <$> getContents
    let keys = map keyToList $ filter isKey schematics
        locks = map lockToList $ filter isLock schematics

    print keys
    print locks

    let combis = [(l, k) | l <- locks, k <- keys]

    putStr "part 1: "; print $ length $ filter (uncurry fits) combis
