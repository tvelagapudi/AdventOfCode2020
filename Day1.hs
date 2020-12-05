import Data.List (find)

doMath :: Int -> [Int] -> Int
doMath fin nums = case nums of
    (n:ns) -> case find ((== fin) . (+ n)) ns of
        Nothing -> doMath fin ns
        Just a -> n * a
    _ -> 0

doMath' :: [Int] -> Int
doMath' nums = case nums of
    (n:ns)
        | doMath (2020 - n) ns == 0 -> doMath' ns
        | otherwise                 -> n * doMath (2020 - n) ns
    _ -> 0

main :: IO ()
main = do
    input <- readFile "Input/Day1Input.txt"
    let stuff func = print . func .  map read . lines $ input
    stuff $ doMath 2020
    stuff doMath'