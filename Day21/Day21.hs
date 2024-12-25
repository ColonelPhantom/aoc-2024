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

type Coord = (Int, Int)
numpad :: M.Map Char Coord
numpad = M.fromList [('7', (0,0)), ('8', (1,0)), ('9', (2,0)), ('4', (0,1)), ('5', (1,1)), ('6', (2,1)), ('1', (0,2)), ('2', (1,2)), ('3', (2,2)), ('0', (1,3)), ('A', (2,3))]

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

dirpadF :: Command -> Command -> [Sequence]
dirpadF from to = [dirpad1 from to]

dirpad1 :: Command -> Command -> Sequence
dirpad1 from to = ((++[Press]) . map readCmd) (buttons2 (head $ show from) (head $ show to))


dirpad :: M.Map Command Coord
dirpad = M.fromList [(Up, (1,0)), (Press, (2,0)), (Left, (0,1)), (Down, (1,1)), (Right, (2,1))]

pathBetweenTwo :: Ord a => M.Map a Coord -> a -> a -> [[Command]]
pathBetweenTwo m startCmd endCmd = go (x'-x, y'-y) where
    (x, y) = m M.! startCmd
    (x', y') = m M.! endCmd

    invMap = S.fromList $ M.elems m

    -- Observation: the path is always one or two straight lines
    go (dx,dy) = nub (horsFirst ++ vertsFirst) where
        horsFirst = if horsFirstOk then [hors ++ verts ++ [Press]] else []
        vertsFirst = if vertsFirstOk then [verts ++ hors ++ [Press]] else []
        horsFirstOk = S.member (x', y) invMap
        vertsFirstOk = S.member (x, y') invMap
        hors
            | dx < 0 = replicate (abs dx) Left
            | dx == 0 = []
            | dx > 0 = replicate (abs dx) Right
        verts
            | dy < 0 = replicate (abs dy) Up
            | dy == 0 = []
            | dy > 0 = replicate (abs dy) Down

allPathBetweenTwo :: Ord a => M.Map a Coord -> a -> a -> [[Command]]
allPathBetweenTwo m startCmd endCmd = go (x, y) (x'-x, y'-y) where
    (x, y) = m M.! startCmd
    (x', y') = m M.! endCmd

    invMap = S.fromList $ M.elems m

    -- Observation: the path is always one or two straight lines
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

shortest :: Ord a => [[a]] -> [[a]]
shortest xs = let l = minimum (map length xs) in filter ((== l) . length) xs

type Pad a = a -> a -> [Sequence]
type Sequence = [Command]

primifySeq :: Sequence -> Sequence'
primifySeq xs@(x:_) = M.fromListWith (+) $ map (,1) $ zip xs (tail xs)

-- elevatePrime :: Pad Command -> Sequence' -> Sequence'
-- elevatePrime f (Prime head sequ) = Prime newHead $ M.fromListWith (+) $ heads ++ concatMap go (M.toList sequ) where
--     start = dirpad1 Press head
--     newHead = case start of
--         [] -> head
--         (x:_) -> x
--     heads = map (,1) $ zip start (tail start)
--     go :: ((Command, Command), Int) -> [((Command, Command), Int)]
--     go ((from, to), count) = map (,count) $ (\xs -> zip xs (tail xs)) $ dirpad1 from to

shortestPrime :: [Sequence'] -> [Sequence']
shortestPrime xs = let l = minimum (map sum xs) in filter ((== l) . sum) xs where

elevatePath :: Pad a -> [a] -> [Sequence]
elevatePath f xs = go $ zip xs (tail xs) where
    go [] = [[]]
    go ((from, to):xs) = (++) <$> f from to <*> go xs

allElevatePath :: Pad a -> [a] -> [Sequence]
allElevatePath f xs = go $ zip xs (tail xs) where
    go [] = [[]]
    go ((from, to):xs) = (++) <$> f from to <*> go xs


-- elevatePathN :: Pad a -> Int -> [Sequence'] -> [Sequence']
-- elevatePathN m 1 xs = primifySeq <$> allElevatePath m xs
-- elevatePathN m n xs = concatMap (elevatePrime dirpadF . (Press:)) targets where
-- elevatePathN m n xs = map (elevatePrime dirpadF) targets where
    -- targets = elevatePathN m (n-1) xs

-- part1 :: [Char] -> ([Char], Int, Int)
-- part1 code = (code, len, n) where
-- solve :: Int -> String -> Int
-- solve complexity code = len * n where
--     len = minimum $ map sum $ shortestPrime $ elevatePathN numpadF complexity ('A':code)
--     n = read $ filter isDigit code

-- MAP ATTEMPT
-- data Sequence' = Prime { first :: Command, counts :: (M.Map (Command, Command) Int) } deriving Show
type Sequence' = M.Map (Command, Command) Int

initializeSequence :: String -> [Sequence']
initializeSequence code = map primifySeq $ elevatePath numpadF ('A':code)

stepSequence :: [Sequence'] -> [Sequence']
stepSequence = map elevateNumpad

elevateNumpad :: Sequence' -> Sequence'
elevateNumpad sequ = M.fromListWith (+) $ concatMap go (M.toList sequ) where
go :: ((Command, Command), Int) -> [((Command, Command), Int)]
go ((from, to), count) = let cmds = Press : dirpad1 from to in map (,count) $ zip cmds (tail cmds)

applyN :: (a -> a) -> Int -> a -> a
applyN f n x = iterate f x !! n

-- solve :: Int -> String -> Int
solve complexity code = (code, bestScore, minimumBy (compare `on` (score)) seqs) where
    seqs = applyN stepSequence complexity $ initializeSequence (code)
    score seq' = sum $ M.elems seq'
    bestScore = minimum $ map (score) seqs
    n = read $ filter isDigit code :: Int

-- DATA.SEQUENCE ATTEMPT
type Seq = Seq.Seq Command

initSeq :: String -> [Seq]
initSeq code = map Seq.fromList $ elevatePath numpadF ('A':code)

stepSeq :: [Seq] -> [Seq]
stepSeq = map elevateNumpad where
    elevateNumpad :: Seq -> Seq
    elevateNumpad sequ = let pairs = Seq.zip (Press Seq.<| sequ) sequ in
        pairs >>= Seq.fromList . uncurry dirpad1

solveSeq complexity code = (code, bestScore, concatMap show $ minimumBy (compare `on` Seq.length) seqs) where
-- solveSeq complexity code = bestScore * n where
    seqs = applyN stepSeq complexity $ initSeq code
    bestScore = minimum $ map Seq.length seqs
    n = read $ filter isDigit code :: Int

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

-- solveSegs complexity code = (code, bestScore, minimumBy (compare `on` score) segs) where
solveSegs complexity code = bestScore * n where
    segs = applyN stepSegs complexity $ initSegs code
    score segmap = sum $ map (\(k,v) -> length k * v) $ M.toList segmap
    bestScore = minimum $ map score segs
    n = read $ filter isDigit code :: Int



main :: IO ()
main = do
    input <- lines <$> getContents
    -- putStr "part 1: "; print $ sum $ map (solve 3) input
    -- putStr "part n: "; print $ sum $ map (solve 4) input
    -- putStr "part 2: "; print $ sum $ map (solve 26) input

    forM_ [0..25] $ \n -> do
        putStr (show n); putStr ": "
        print $ sum $ map (solveSegs n) input
    -- mapM_ (print. concatMap show) $ elevatePathN numpad 3 "A029A"
    -- mapM_ (print. concatMap show) $ elevatePathN numpad 2 "A029A"
    -- print $ length $ elevatePathN numpad 3 "A029A"