parse :: String -> [Bool]
parse = map (== '#') 

checkVal :: [Bool] -> Int -> Bool
checkVal lin spac = lin !! (spac `mod` length lin)

stepR :: Int -> Int -> [[Bool]] -> [Bool]
stepR index change (v:vs) = checkVal v index : stepR (index + change) change vs
stepR _ _ _ = []

listDiv :: Int -> [a] -> [a]
listDiv spl lis@(_:_) = (head . take spl) lis : (listDiv spl . drop spl) lis
listDiv _ _            = []

findTrees :: [[Bool]] -> (Int, Int) -> Int
findTrees input (r, d) = length . filter (== True) . stepR 0 r . listDiv d $ input

main :: IO ()
main = do
    input <- map parse <$> lines <$> readFile "Input/Day3Input.txt"
    print . findTrees input $ (3, 1)
    print . product . map (findTrees input) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]