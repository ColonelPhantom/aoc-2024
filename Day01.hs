import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    (left, right) <- unzip . map (\x -> let [a,b] = words x in (read a, read b)) . lines <$> getContents
    let dist = sum $ zipWith (\x y -> abs (x-y)) (sort left) (sort right)
    putStr "Part 1: " >> print dist

    let rmap = M.fromListWith (+) $ map (, 1) right
    let simf x = fromMaybe 0 $ M.lookup x rmap
    let sim = sum $ map (\x -> x * simf x) left
    putStr "Part 2: " >> print sim
