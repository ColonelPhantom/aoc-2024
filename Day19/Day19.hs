import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Array as A
import Debug.Trace (traceShow)

readTowels :: String -> [String]
readTowels = splitOn ", "

tryTowelM :: [String] -> String -> Int
tryTowelM towels pattern = a A.! length pattern where
    a = A.array (0, length pattern) $ [(i, go i) | i <- [0..length pattern]]
    go :: Int -> Int
    go 0 = 1
    go i = sum $ mapMaybe go1 towels where
        p = drop (length pattern - i) pattern
        try1 :: String -> Maybe Int
        try1 t = if t == take (length t) p then Just (i - length t) else Nothing
        go1 :: String -> Maybe Int
        go1 t = fmap (a A.!) (try1 t)


main = do
    (towelsLine : _ : patterns) <- lines <$> getContents
    let towels = readTowels towelsLine

    let solutions = map (tryTowelM towels) patterns

    putStr "part 1: "; print $ length $ filter (>0) solutions
    putStr "part 2: "; print $ sum solutions