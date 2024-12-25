import Prelude hiding (Left, Right)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.List (nub, minimumBy)
import qualified Data.Set as S
import Data.Foldable (toList)
import Data.Function (on)
import Data.List.Split (splitOn)
import Debug.Trace

-- NUMPAD
type Coord = (Int, Int)
numpad :: M.Map Char Coord
numpad = M.fromList [('7', (0,0)), ('8', (1,0)), ('9', (2,0)), ('4', (0,1)), ('5', (1,1)), ('6', (2,1)), ('1', (0,2)), ('2', (1,2)), ('3', (2,2)), ('0', (1,3)), ('A', (2,3))]

allPathBetweenTwo :: Ord a => M.Map a Coord -> a -> a -> [[Command]]
allPathBetweenTwo m startCmd endCmd = go (x, y) (x'-x, y'-y) where
    (x, y) = m M.! startCmd
    (x', y') = m M.! endCmd

    invMap = S.fromList $ M.elems m

    go (sx, sy) (0,0) = [[Press]]
    go (sx, sy) (dx,dy) = horts ++ verts where
        horts
            | dx < 0 = if S.member (sx-1, sy) invMap then map (Left :) $ go (sx-1, sy) (dx+1, dy) else []
            | dx == 0 = []
            | dx > 0 = if S.member (sx+1, sy) invMap then map (Right :) $ go (sx+1, sy) (dx-1, dy) else []
        verts
            | dy < 0 = if S.member (sx, sy-1) invMap then map (Up :) $ go (sx, sy-1) (dx, dy+1) else []
            | dy == 0 = []
            | dy > 0 = if S.member (sx, sy+1) invMap then map (Down :) $ go (sx, sy+1) (dx, dy-1) else []

numpadF :: Char -> Char -> [Sequence]
numpadF = allPathBetweenTwo numpad

-- DIRPAD
data Command = Up | Down | Left | Right | Press deriving (Eq, Ord)
instance Show Command where
    show Up = "^"
    show Down = "v"
    show Left = "<"
    show Right = ">"
    show Press = "A"
readCmd :: Char -> Command
readCmd '^' = Up
readCmd 'v' = Down
readCmd '<' = Left
readCmd '>' = Right
readCmd 'A' = Press

-- Pre-computed best button presses
buttons2 :: Char -> Char -> String
buttons2 'A' 'A' = ""
buttons2 'A' '^' = "<"
buttons2 'A' '>' = "v"
buttons2 'A' 'v' = "<v"
buttons2 'A' '<' = "v<<"
buttons2 '^' '^' = ""
buttons2 '^' 'A' = ">"
buttons2 '^' 'v' = "v"
buttons2 '^' '<' = "v<"
buttons2 '^' '>' = "v>"
buttons2 'v' 'v' = ""
buttons2 'v' 'A' = "^>"
buttons2 'v' '^' = "^"
buttons2 'v' '<' = "<"
buttons2 'v' '>' = ">"
buttons2 '<' '<' = ""
buttons2 '<' 'A' = ">>^"
buttons2 '<' '^' = ">^"
buttons2 '<' 'v' = ">"
buttons2 '<' '>' = ">>"
buttons2 '>' '>' = ""
buttons2 '>' 'A' = "^"
buttons2 '>' '^' = "<^"
buttons2 '>' 'v' = "<"
buttons2 '>' '<' = "<<"

dirpad1 :: Command -> Command -> Sequence
dirpad1 from to = ((++[Press]) . map readCmd) (buttons2 (head $ show from) (head $ show to))

-- PATHS
type Sequence = [Command]
elevatePath :: (a -> a -> [Sequence]) -> [a] -> [Sequence]
elevatePath f xs = go $ zip xs (tail xs) where
    go [] = [[]]
    go ((from, to):xs) = (++) <$> f from to <*> go xs

-- SEGMENT MAP APPROACH 
type Segs = M.Map [Command] Int

initSegs :: String -> [Segs]
initSegs code = map (M.fromListWith (+) . map (,1) . map (++ [Press]). init . splitOn [Press]) (elevatePath numpadF ('A':code))

stepSegs :: [Segs] -> [Segs]
stepSegs = map elevateNumpad where
    elevateNumpad :: Segs -> Segs
    elevateNumpad segs = M.fromListWith (+) $ concatMap go (M.toList segs)

    traceGo x = let r = go x in traceShow ("go", x, r) r
    go :: ([Command], Int) -> [([Command], Int)]
    go (cmds, count) = zipWith (\from to -> (dirpad1 from to, count)) (Press:cmds) cmds

applyN :: (a -> a) -> Int -> a -> a
applyN f n x = iterate f x !! n

solveSegs :: Int -> [Char] -> Int
solveSegs complexity code = bestScore * n where
    segs = applyN stepSegs complexity $ initSegs code
    score segmap = sum $ map (\(k,v) -> length k * v) $ M.toList segmap
    bestScore = minimum $ map score segs
    n = read $ filter isDigit code :: Int

main :: IO ()
main = do
    input <- lines <$> getContents
    forM_ [0..25] $ \n -> do
        putStr (show n); putStr ": "
        print $ sum $ map (solveSegs n) input
