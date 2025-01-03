import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.List (sort)
import Control.Parallel.Strategies
type Coord = (Int, Int)

data Robot = Robot { pos :: Coord, vel :: Coord } deriving (Show)

readRobot :: String -> Robot
readRobot s = Robot (read px, read py) (read vx, read vy) where
    [p,v] = map (drop 2) $ words s
    [px, py] = splitOn "," p
    [vx, vy] = splitOn "," v

calculateSteps :: Coord -> Int -> Robot -> Coord
calculateSteps (w,h) 0 (Robot (px,py) (vx,vy)) = (px, py)
calculateSteps (w,h) n (Robot (px,py) v@(vx,vy)) = calculateSteps (w,h) (n-1) (Robot ((px+vx) `mod `w, (py+vy) `mod` h) v)
calculateSteps' (w,h) n (Robot (px,py) v@(vx,vy)) = (x `mod` w, y `mod` h) where
    x = px + (n*vx)
    y = py + (n*vy)

data Quad = NW | NE | SE | SW deriving (Show, Eq)
findQuadrant :: Coord -> Coord -> Maybe Quad
findQuadrant (w,h) (x,y) = case (compare x middleW, compare y middleH) of
        (LT, LT) -> Just NW
        (LT, GT) -> Just NE
        (GT, LT) -> Just SE
        (GT, GT) -> Just SW
        _ -> Nothing
     where
    middleH = h `div` 2
    middleW = w `div` 2

part1 :: Coord -> Int -> [Robot] -> Int
part1 (w,h) steps robots = total where
    positions = map (calculateSteps' (w,h) 100) robots
    quadrants = mapMaybe (findQuadrant (w,h)) positions
    counts = map (\q -> length (filter (==q) quadrants)) [NW, NE, SE, SW]
    total = product counts

hasDups :: Ord a => [a] -> Bool
hasDups xs = go S.empty xs where
    go s [] = False
    go s (x:xs) = if S.member x s then True else go (S.insert x s) xs

findNoOverlap :: [Robot] -> Int
findNoOverlap rs = length (takeWhile id xs) where
    xs = map (\n -> hasDups $ map (calculateSteps' (101,103) n) rs) [0..] `using` parBuffer 1024 rseq

main :: IO ()
main = do
    input <- lines <$> getContents
    let robots = map readRobot input

    putStr "part 1 example: " >> print (part1 (11,7) 100 robots)
    putStr "part 1: " >> print (part1 (101,103) 100 robots)

    putStr "part 2: "; print $ findNoOverlap robots
    -- let robotPosExample = map (calculateSteps (11,7) 100) robots
    -- mapM_ (print . calculateSteps (11,7) 100) robots
