import Control.Monad (zipWithM)
import Data.List (sortOn)
import Data.List.Split (splitOn)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
 
euclid :: Int -> Int -> (Int, Int)
euclid _ 0 = (1, 0)
euclid a b = (t, s - q * t)
  where
    (s, t) = euclid b r
    (q, r) = a `quotRem` b
 
chineseRem :: [Int] -> [Int] -> Int
chineseRem res mods = (`mod` modsP) . sum $ zipWith3 (\a b c -> a * b * c) crtMods res
    $ map fst $ zipWith euclid crtMods mods where 
        modsP = product mods
        crtMods = map (modsP `div`) mods

parse :: [String] -> [(Int, Int)]
parse strs = map rd . filter ((/= "x") . snd) $ zip [0..(length strs - 1)] strs where
    rd (a, b) = (mod' a (read b), (read b))

mod' :: Int -> Int -> Int
mod' n p = p - (n `mod` p)

firstBus :: Int -> [Int] -> Int
firstBus n = (\(x,y) -> x * y) . head . sortOn snd . map ((mod' n <$>) . pairify) where
    pairify a = (a, a)

main :: IO ()
main = do
    input <- lines <$> readFile "Input/Day13Input.txt"
    let arrival = read $ input !! 0
        times   = unzip . parse . splitOn "," $ input !! 1
    print . firstBus arrival . snd $ times
    print . uncurry chineseRem     $ times