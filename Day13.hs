import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Coord = (Integer, Integer)
data Problem = Problem { getA :: Coord, getB :: Coord, goal :: Coord} deriving (Eq, Show)

parse :: [String] -> Problem
parse [aStr, bStr, goalStr] = Problem (read xA, read yA) (read xB, read yB) (read xG, read yG) where
    [xA', yA] = map (drop 2) $ tail $ tail $ words aStr
    [xB', yB] = map (drop 2) $ tail $ tail $ words bStr
    [xG', yG] = map (drop 2) $ tail $ words goalStr
    xA = init xA'
    xB = init xB'
    xG = init xG'

cost :: Coord -> Integer
cost (a,b) = 3*a + b

solve :: Problem -> Maybe Coord
solve (Problem (xA,yA) (xB,yB) (xGoal,yGoal)) = if realA == xGoal && real == yGoal then Just solved else Nothing
    where
    x = (xGoal - xB*y) `div` xA
    y = (xGoal*yA - xA*yGoal) `div` (xB*yA - xA*yB)
    realA = xA*x + xB*y
    real = yA*x + yB*y
    solved = (x,y)

part1 :: Problem -> Maybe Integer
part1 p = solve p >>= \(a,b) -> if a <= 100 && b <= 100 then Just $ cost (a,b) else Nothing

part2 :: Problem -> Maybe Integer
part2 (Problem a b (xG, yG)) = fmap cost $ solve $ Problem a b (10000000000000+xG, 10000000000000+yG)

main :: IO ()
main = do
    input <- map parse . splitOn [""] . lines <$> getContents
    putStr "part 1: " >> print (sum $ mapMaybe part1 input)
    putStr "part 2: " >> print (sum $ mapMaybe part2 input)
