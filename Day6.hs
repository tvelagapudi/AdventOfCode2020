import Data.List (nub, intersect)
import Data.List.Split (splitOn)

countAny :: String -> Int
countAny = length . nub . filter (/= '\n')

countAll :: String -> Int
countAll = length . intersection . lines

intersection :: Eq a => [[a]] -> [a]
intersection = foldr1 intersect

main :: IO ()
main = do
    input <- splitOn "\n\n" <$> readFile "Input/Day6Input.txt"
    print . sum . map countAny $ input
    print . sum . map countAll $ input