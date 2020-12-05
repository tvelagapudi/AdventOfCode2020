import Text.ParserCombinators.ReadP
import Data.Char

parse :: String -> Int
parse = seatID . map (fromEnum . (\c -> c == 'R' || c == 'B'))

main :: IO ()
main = do
    input <- map parse <$> lines <$> readFile "Input/Day5Input.txt"
    print $ maximum input
    let allSeats = [8 * x + y | x <- [0..127], y <- [0..7]]
    print $ findAdjacent (allSeats \\ input) input