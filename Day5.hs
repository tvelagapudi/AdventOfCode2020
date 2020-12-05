import Data.List ((\\))

seatID :: [Int] -> Int
seatID = sum . (<*>) [(8 *) . binConvert . take 7, binConvert . drop 7] . pure

binConvert :: [Int] -> Int
binConvert input@(n:ns) = n * 2 ^ (length input - 1) + binConvert ns
binConvert _ = 0

parse :: String -> Int
parse = seatID . map (fromEnum . (\c -> c == 'R' || c == 'B'))

findAdjacent :: [Int] -> [Int] -> Int
findAdjacent (n:ns) list
    | all (`elem` list) [n+1, n-1] = n
    | otherwise = findAdjacent ns list
findAdjacent _ _ = 100000

main :: IO ()
main = do
    input <- map parse <$> lines <$> readFile "Input/Day5Input.txt"
    print $ maximum input
    let allSeats = [8 * x + y | x <- [0..127], y <- [0..7]]
    print $ findAdjacent (allSeats \\ input) input