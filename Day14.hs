import Text.ParserCombinators.ReadP
import Data.List (sortOn, groupBy, elemIndices)
import Data.Char (isAlphaNum, isDigit)

data Memory = Mem (Int, Int) deriving (Eq, Show)

newtype Mask = Mask String deriving (Eq, Show)

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
    _ -> error (x:xs)
fromBinary _ = 0

genFunc :: String -> [String]
genFunc s
    | null val  = [s]
    | otherwise = [take (head val) s ++ [x] ++ rest | x <- ['0', '1'], rest <- genFunc (drop (head val + 1) s)] where
        val = elemIndices 'X' s

specialFunc :: String -> [Int]
specialFunc str = map fromBinary . genFunc $ str

func :: Mask -> Memory -> Memory
func (Mask a) (Mem (b, c)) = Mem (b, fromBinary (zipWith f a large)) where
    large = replicate (length a - length bin) '0' ++ bin
    bin = toBinary c
    f 'X' y = y
    f z y = z

func' :: Mask -> Memory -> [Memory]
func' (Mask a) (Mem (b, c)) = map (Mem . flip (,) c) (specialFunc $ zipWith f a large) where
    large = replicate (length a - length bin) '0' ++ bin
    bin = toBinary b
    f '0' y = y
    f z y = z

doThings :: (Mask, [Memory]) -> [Memory]
doThings (a, b) = map (func a) b

doAltThings :: (Mask, [Memory]) -> [Memory]
doAltThings (a, b) = concat $ map (func' a) b

fst' :: Memory -> Int
fst' (Mem (a,b)) = a

snd' :: Memory -> Int
snd' (Mem (a,b)) = b

main :: IO ()
main = do
    input <- lines <$> readFile "Input/Day14Input.txt"
    let vals = concat . map (doThings . parse) . splitByMasks $ input
        vals' = concat . map (doAltThings . parse) . splitByMasks $ input
    print . sum . map (snd' . last) . groupBy (\x y -> fst' x == fst' y) . sortOn fst' $ vals
    print . sum . map (snd' . last) . groupBy (\x y -> fst' x == fst' y) . sortOn fst' $ vals'