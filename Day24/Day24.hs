{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Function (fix)
import Data.List (sort, sortOn, intercalate)
import Data.Char (isDigit)

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

decode :: M.Map String Bool -> Int
decode m = sum $ map (uncurry go) $ M.toList m where
    go :: String -> Bool -> Int
    go ('z':num) val = if val then 2 ^ read num else 0
    go _ _ = 0

eval :: [Const] -> [Gate] -> M.Map String Bool
eval consts gates = fix $ \m -> M.map (\f -> f m) $ M.fromList $ map evalConst consts ++ map evalGate gates

-- Part 2
uses :: String -> Gate -> Bool
uses x (Gate typ a b o ) = x `elem` [a,b]

ofTyp :: String -> [Gate] -> [Gate]
ofTyp typ = filter (\(Gate t _ _ _) -> t == typ)

usedIn :: String -> String -> [Gate] -> Bool
usedIn typ nm gs = any (uses nm) (ofTyp typ gs)

isDirect :: String -> Bool
isDirect ('x':xs) = all isDigit xs || error "boom"
isDirect ('y':xs) = all isDigit xs || error "boom"
isDirect ('z':xs) = all isDigit xs || error "boom"
isDirect _ = False

isIndirect = not . isDirect

isSafe :: [Gate] -> Gate -> Bool
isSafe gs (Gate "XOR" "x00" "y00" "z00") = True
isSafe gs (Gate "XOR" "y00" "x00" "z00") = True
isSafe gs (Gate "XOR" a b o) = (isIndirect a && isIndirect b && isDirect o)
                                || (isDirect a && isDirect b && isIndirect o && (usedIn "AND" o gs || usedIn "XOR" o gs))
isSafe gs (Gate "OR" a b "z45") = isIndirect a && isIndirect b
isSafe gs (Gate "OR" a b o) = all isIndirect [a,b,o] && usedIn "AND" o gs && usedIn "XOR" o gs

isSafe gs (Gate "AND" "x00" "y00" o) = True
isSafe gs (Gate "AND" a b o) = isIndirect o && (all isDirect [a,b] || all isIndirect [a,b]) && usedIn "OR" o gs

main :: IO ()
main = do
    [constsS, gatesS] <- splitOn [""] . lines <$> getContents
    let consts = map readConst constsS
    let gates = map readGate gatesS

    putStr "part 1: "; print $ decode $ eval consts gates

    putStr "part 2: "; putStrLn $ intercalate "," $ sort $ map output $ filter (not . isSafe gates) gates