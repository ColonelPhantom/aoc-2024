import Data.Char (isDigit)

data Instr = Mul Integer Integer | Do | Dont deriving (Show, Eq)

getMuls :: String -> [Instr]
getMuls [] = []
getMuls xs@(a : as)
  | mul == "mul(" && comma == "," && end == ")" = Mul (read a) (read b) : getMuls as
  | take 4 xs == "do()" = Do : getMuls as
  | take 7 xs == "don't()" = Dont : getMuls as
  | otherwise = getMuls as
  where
      (mul, mtl) = splitAt 4 xs
      (a, atl) = span isDigit mtl
      (comma, ctl) = splitAt 1 atl
      (b, btl) = span isDigit ctl
      (end, etl) = splitAt 1 btl

eval1 :: [Instr] -> Integer
eval1 [] = 0
eval1 (Mul a b : xs) = a*b + eval1 xs
eval1 (x:xs) = eval1 xs


eval2 :: Bool -> [Instr] -> Integer
eval2 _ [] = 0
eval2 b (Dont : xs) = eval2 False xs
eval2 b (Do : xs) = eval2 True xs
eval2 False (Mul a b : xs) = eval2 False xs
eval2 True (Mul a b : xs) = a*b + eval2 True xs

main :: IO ()
main = do
    input <- getMuls <$> getContents
    print input
    putStr "Part 1: " >> print (eval1 input)
    putStr "Part 2: " >> print (eval2 True input)
