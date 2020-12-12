import Data.Map.Strict hiding (filter, map)

type Grid = Map (Int, Int) Char

checkAdj :: Int -> Char -> String -> Char
checkAdj num pos adjacent
    | pos == 'L' && (length . filter (== '#') $ adjacent) == 0   = '#'
    | pos == '#' && (length . filter (== '#') $ adjacent) >= num = 'L'
    | otherwise = pos

inGrid :: (Int, Int) -> Grid -> Char
inGrid coord gd
    | coord `member` gd = gd ! coord
    | otherwise         = 'L'

findAdj :: Grid -> (Int, Int) -> String
findAdj gd  (x, y) = [inGrid (x', y') gd | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]

findDir :: Int -> Grid -> (Int, Int) -> String
findDir m gd (x,y) = [findSeat [inGrid (x + x'*a, y + y'*a) gd | a <- [1..m]]
    | x' <- [-1..1], y' <- [-1..1], (x',y') /= (0,0)] where
        findSeat  = head . filter (/= '.')

step :: (Grid -> (Int, Int) -> String) -> Int -> Grid -> Grid
step f n gd = fromList . zip ks . zipWith (checkAdj n) (elems gd) . map (f gd) $ ks where
    ks = keys gd

run :: (Grid -> Grid) -> Grid -> Grid
run f gd
    | new == gd = gd
    | otherwise = run f new where
        new = f gd

main :: IO ()
main = do
    input <- lines <$> readFile "Input/Day11Input.txt"
    let grid = fromList [((x, y), (input !! y) !! x) | 
            x <- [0..((length . head $ input) - 1)], y <- [0..(length input - 1)]]
    print . length . filter (== '#') . elems . run (step  findAdj     4) $ grid
    print . length . filter (== '#') . elems . run (step (findDir 97) 5) $ grid