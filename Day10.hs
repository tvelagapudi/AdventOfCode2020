import Control.Monad.State
import Data.List (sort, nub)
import Data.Map.Strict hiding (filter, map)

giveDifferences :: Int -> [Int] -> [Int]
giveDifferences lst (j:js) = (j - lst) : giveDifferences j js
giveDifferences _ _ = []

numPaths :: [Int] -> Int -> State (Map Int Int) Int
numPaths as j
    | j == 0 = pure 1
    | otherwise = do
        prev <- get
        case prev !? j of
            Nothing -> do
                paths <- sequence [numPaths as j' | j' <- takeWhile (< j) as, j' >= j - 3]
                put $ insert j (sum paths) prev
                pure . sum $ paths
            Just a -> pure a

main :: IO ()
main = do
    input <- map read . lines <$> readFile "Input/Day10Input.txt"
    let phone = maximum input + 3
        adapters = [0] ++ sort (nub (input)) ++ [phone]
        f diff = length . filter (== diff) . giveDifferences 0 $ adapters
    print $ f 1 * f 3
    print $ evalState (numPaths adapters phone) empty