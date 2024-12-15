import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.List (sort)
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

findNoOverlap :: [Robot] -> Int
findNoOverlap rs = go 0 where
    go n = let rs' = map (calculateSteps' (101,103) n) rs in traceShow n $ if S.toAscList (S.fromList rs') == sort rs' then n else go (n+1)

main :: IO ()
main = do
    input <- lines <$> getContents
    let robots = map readRobot input

    putStr "Part 1 example: " >> print (part1 (11,7) 100 robots)
    putStr "Part 1: " >> print (part1 (101,103) 100 robots)

    print $ findNoOverlap robots
    -- let robotPosExample = map (calculateSteps (11,7) 100) robots
    -- mapM_ (print . calculateSteps (11,7) 100) robots
