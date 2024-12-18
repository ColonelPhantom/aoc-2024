import Data.Bits (xor)
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Data.List (intersperse, intercalate)

-- Input handling
data State = State {
    getA :: Int,
    getB :: Int,
    getC :: Int,
    getIP :: Int,
    output :: Seq.Seq Int
} deriving (Show)

parseState :: [String] -> (State, [Int])
parseState [as, bs, cs, _, ps] = (State a b c 0 Seq.Empty, p) where
    a = read (last $ words as)
    b = read (last $ words bs)
    c = read (last $ words cs)
    p = map read $ splitOn "," $ last (words ps)

-- Part 1
combo :: State -> Int -> Int
combo s 0 = 0
combo s 1 = 1
combo s 2 = 2
combo s 3 = 3
combo s 4 = getA s
combo s 5 = getB s
combo s 6 = getC s
combo s 7 = error "reserved operand 7"

dv :: State -> Int -> Int
dv s c = getA s `div` (2 ^ combo s c)

execOp :: State -> Int -> Int -> State
execOp s 0 o = s { getA = dv s o } -- adv
execOp s 1 o = s { getB = getB s `xor` o } -- bxl
execOp s 2 o = s { getB = combo s o `mod` 8 } -- bst
execOp s 3 o = if getA s == 0 then s else s { getIP = o - 2 } -- jnz
execOp s 4 o = s { getB = getB s `xor` getC s } -- bxc
execOp s 5 o = s { output = output s Seq.|> (combo s o `mod ` 8) } -- out
execOp s 6 o = s { getB = dv s o } -- bdv
execOp s 7 o = s { getC = dv s o } -- cdv

exec :: Seq.Seq Int -> State -> State
exec prog s = fromMaybe s $ do
    instr <- prog Seq.!? getIP s
    oper <- prog Seq.!? (getIP s + 1)
    let s' = execOp s instr oper
    return $ exec prog $ s' { getIP = getIP s' + 2 }

-- Part 2

-- Run program with given A 'a', and check if output is equal to goal
testA :: Seq.Seq Int -> [Int] -> Int -> Bool
testA p goal a = toList (output $ exec p $ State a 0 0 0 Seq.Empty) == goal

-- In our program, each iteration divides by 8.
-- As such, we need the next iteration to start at an 'a' so that 'a `div` 8' == recoverA p xs',
-- which is true for 8*a through 8*a + 7
recoverA :: Seq.Seq Int -> [Int] -> [Int]
recoverA p goal = filter (testA p goal) $ case goal of
        [] -> error "empty goal"
        [x] -> [0..7]
        (x:xs) -> concatMap (\a -> [8*a .. 8*a + 7]) (recoverA p xs)

main :: IO ()
main = do
    (initState, program) <- parseState . lines <$> getContents
    putStr "part 1: "; putStrLn $ intercalate "," (map show $ toList $ output $ exec (Seq.fromList program) initState)
    putStr "part 2: "; print $ minimum $ recoverA (Seq.fromList program) program

