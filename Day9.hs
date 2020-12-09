isSum :: Int -> [Int] -> Bool
isSum val ls = or $ map (\(x,y) -> val == x + y) [(x, y) | x <- ls, y <- ls, (x /= y)]

findError :: [Int] -> [Int] -> Int
findError lstTF (n:ns)
    | n `isSum` lstTF = findError (tail lstTF ++ [n]) ns
    | otherwise = n
findError lstTF _ = 0

takeUntil :: Int -> [Int] -> [Int]
takeUntil val (n:ns)
    | val - n < 0 = []
    | otherwise   = n : takeUntil (val - n) ns
takeUntil _ _ = []

encryptWeak :: Int -> [Int] -> Int
encryptWeak val (n:ns)
    | val == sum chk && length chk > 1 = maximum chk + minimum chk
    | otherwise = encryptWeak val ns where
        chk = takeUntil val $ n:ns
encryptWeak _ _ = 0

main :: IO ()
main = do
    input <- map read . lines <$> readFile "Input/Day9Input.txt" :: IO [Int]
    let incorrect = uncurry findError . splitAt 25 $ input
    print incorrect
    print . encryptWeak incorrect $ input