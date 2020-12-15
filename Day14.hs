import Text.ParserCombinators.ReadP
import Data.List (sortOn, groupBy, elemIndices)
import Data.Char (isAlphaNum, isDigit)

data Memory = Mem (Int, Int) deriving (Eq, Show)

newtype Mask = Mask String deriving (Eq, Show)

app :: ((Int, Int) -> c) -> Memory -> c
app f (Mem x) = f x

parseMask :: ReadP Mask
parseMask = Mask <$
    string "mask = "
    <*> munch1 isAlphaNum

parseMem :: ReadP Memory
parseMem = fmap Mem $ (,) <$
    string "mem["
    <*> (read <$> munch1 isDigit)
    <* string "] = "
    <*> (read <$> munch1 isDigit)

instance Read Memory where
    readsPrec _ = readP_to_S parseMem

instance Read Mask where
    readsPrec _ = readP_to_S parseMask

splitByMasks :: [String] -> [[String]]
splitByMasks [] = []
splitByMasks strs = (head strs : mems) : splitByMasks rest where
    mems = takeWhile ((== "mem") . (take 3)) (tail strs)
    rest = dropWhile ((== "mem") . (take 3)) (tail strs)

parse :: [String] -> (Mask, [Memory])
parse input = (read (head input), map read (tail input))

toBinary :: Int -> String
toBinary 0 = ""
toBinary input = toBinary a ++ show b where
    (a, b) = quotRem input 2

fromBinary :: String -> Int
fromBinary (x:xs) = case x of
    '0' -> fromBinary xs
    '1' -> 2 ^ (length xs) + fromBinary xs
fromBinary _ = 0

genBin :: String -> [String]
genBin s
    | null val  = [s]
    | otherwise = [take (head val) s ++ [x] ++ rest | x <- ['0', '1'], 
        rest <- genBin (drop (head val + 1) s)] where
            val = elemIndices 'X' s

sumChoices :: String -> [Int]
sumChoices str = map fromBinary . genBin $ str

mask :: Mask -> Memory -> Memory
mask (Mask a) (Mem (b, c)) = Mem (b, fromBinary (zipWith f a large)) where
    large = replicate (length a - length bin) '0' ++ bin
    bin = toBinary c
    f 'X' y = y
    f z y = z

maskV2 :: Mask -> Memory -> [Memory]
maskV2 (Mask a) (Mem (b, c)) = map (Mem . flip (,) c) (sumChoices $ zipWith f a large) where
    large = replicate (length a - length bin) '0' ++ bin
    bin = toBinary b
    f '0' y = y
    f z y = z

maskAll :: (Mask, [Memory]) -> [Memory]
maskAll (a, b) = map (mask a) b

maskAllV2 :: (Mask, [Memory]) -> [Memory]
maskAllV2 (a, b) = concat $ map (maskV2 a) b

main :: IO ()
main = do
    input <- lines <$> readFile "Input/Day14Input.txt"
    let vals = concat . map (maskAll . parse) . splitByMasks $ input
        vals' = concat . map (maskAllV2 . parse) . splitByMasks $ input
        fst' = app fst
        snd' = app snd
        match x y = fst' x == fst' y
    print . sum . map (snd' . last) . groupBy match . sortOn fst' $ vals
    print . sum . map (snd' . last) . groupBy match . sortOn fst' $ vals'