import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Array as A
import Debug.Trace (traceShow)

readTowels :: String -> [String]
readTowels = splitOn ", "

tryTowel :: [String] -> String -> [[String]]
tryTowel towels [] = [[]]
tryTowel towels pattern = concat $ mapMaybe go towels
    where
    go :: String -> Maybe [[String]]
    go t = map (t:) . tryTowel towels <$> try1 t pattern
    try1 :: String -> String -> Maybe String
    try1 [] ps = Just ps
    try1 (t:ts) [] = Nothing
    try1 (t:ts) (p:ps) = if t == p then try1 ts ps else Nothing

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

    print towels

    let solutions = map (tryTowelM towels) patterns

    let lengths = solutions

    putStr "part 1: "; print $ length $ filter (>0) solutions
    putStr "part 2: "; print $ sum $ lengths