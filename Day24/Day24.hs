{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bifunctor
import Data.Function (fix)
import Debug.Trace (traceShow)
import Data.Bits (testBit)

type Const = (String, Bool)
readConst :: String -> (String, Bool)
readConst xs = let [a, b] = splitOn ":" xs in (a, read b > 0)

data Gate = Gate
    { gateType :: String
    , inputA :: String
    , inputB :: String
    , output :: String
    } deriving (Show, Eq)

readGate :: String -> Gate
readGate xs = Gate typ a b o where
    [a, typ, b, _, o] = words xs

type Eval = M.Map String Bool -> Bool
evalConst :: (String, Bool) -> (String, Eval)
evalConst (name, val) = (name, const val)

evalGate :: Gate -> (String, Eval)
evalGate (Gate typ a b o) = (o, f) where
    f m = let x = m M.! a; y = m M.! b in case typ of
        "AND" -> x && y
        "OR" -> x || y
        "XOR" -> x /= y

decodeX :: M.Map String Bool -> Int
decodeX m = sum $ map (uncurry go) $ M.toList m where
    go :: String -> Bool -> Int
    go ('x':num) val = if val then 2 ^ read num else 0
    go _ _ = 0

decodeY :: M.Map String Bool -> Int
decodeY m = sum $ map (uncurry go) $ M.toList m where
    go :: String -> Bool -> Int
    go ('y':num) val = if val then 2 ^ read num else 0
    go _ _ = 0

decode :: M.Map String Bool -> Int
decode m = sum $ map (uncurry go) $ M.toList m where
    go :: String -> Bool -> Int
    go ('z':num) val = if val then 2 ^ read num else 0
    go _ _ = 0

type Deps = M.Map String (S.Set String) -> S.Set String
depsConst :: Const -> (String, Deps)
depsConst (name, val) = (name, \m -> S.singleton name)
depsGate :: Gate -> (String, Deps)
depsGate (Gate typ a b o) = (o, \m -> S.insert o $ S.unions [m M.! a, m M.! b])

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

eval :: [Const] -> [Gate] -> M.Map String Bool
eval consts gates = fix $ \m -> M.map (\f -> f m) $ M.fromList $ map evalConst consts ++ map evalGate gates

deps :: [Const] -> [Gate] -> M.Map String (S.Set String)
deps consts gates = fix $ \m -> M.map (\f -> f m) $ M.fromList $ map depsConst consts ++ map depsGate gates

findSwaps :: [String] -> [Const] -> [Gate] -> [(String, String)]
findSwaps blocked c g = [(t, f) | t <- trueGates, f <- falseGates, t `S.notMember` (d M.! f), f `S.notMember` (d M.! t)] where
    m = eval c g
    d = deps c g
    trueGates  = M.keys $ M.filterWithKey (\x v -> x `elem` map output g && x `notElem` blocked && v) m
    falseGates = M.keys $ M.filterWithKey (\x v -> x `elem` map output g && x `notElem` blocked && not v) m

swap :: [Gate] -> (String, String) -> [Gate]
swap gs (x,y) = map go gs where
    go (Gate typ a b o)
        | o == x = Gate typ a b y
        | o == y = Gate typ a b x
        | otherwise = Gate typ a b o

candidates :: [Const] -> [Gate] -> [(String, String, [Gate])]
candidates consts gates = do
        (a,b) <- findSwaps [] consts gates
        return (a,b, swap gates (a,b))

candidates2 :: [Const] -> [Gate] -> [(String, String, String, String, [Gate])]
candidates2 consts gates = do
    (a,b,gates') <- candidates consts gates
    (c,d) <- findSwaps [a,b] consts gates'
    return (a,b,c,d, swap gates' (c,d))

candidates3 :: [Const] -> [Gate] -> [(String, String, String, String, String, String, [Gate])]
candidates3 consts g = do
    (a,b,c,d,gates') <- candidates2 consts g
    (e,f) <- findSwaps [a,b,c,d] consts gates'
    return (a,b,c,d,e,f, swap gates' (e,f))

candidates4 :: [Const] -> [Gate] -> [(String, String, String, String, String, String, String, String, [Gate])]
candidates4 consts gates = do
    (a,b,c,d,e,f,gates') <- candidates3 consts gates
    (g,h) <- findSwaps [a,b,c,d,e,f] consts gates'
    return (a,b,c,d,e,f,g,h, swap gates' (g,h))

-- renameGates :: [Gate] -> [Gate]
-- renameGates gs = map (\g -> m M.! output g) gs where
--     m = M.fromList (\f -> f m) $ map go gs
--     go ('z':num) = let n = read num in undefined



main :: IO ()
main = do
    [constsS, gatesS] <- splitOn [""] . lines <$> getContents
    let consts = map readConst constsS
    let gates = map readGate gatesS

    print $ eval consts gates
    putStr "part 1: "
    print $ decode $ eval consts gates

    let x = decodeX $ eval consts gates
    let y = decodeY $ eval consts gates
    print $ map (\i -> if testBit (x) i then 1 else 0) [0..50]
    print $ map (\i -> if testBit (y) i then 1 else 0) [0..50]
    print $ map (\i -> if testBit (x+y) i then 1 else 0) [0..50]
    print $ map (\i -> if testBit (decode $ eval consts gates) i then 1 else 0) [0..50]

    -- print $ length $ candidates2 consts gates

    print $ length gates
