import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.Char
import Data.Map.Strict hiding (map, filter)

poundAlphaNum :: Char -> Bool
poundAlphaNum = or . (<*>) [(== '#'), isAlphaNum] . pure

inBetween :: (Ord a) => a -> a -> a -> Bool
inBetween min max = and . (<*>) [(min <=), (max >=)] . pure

data Passport = Passport (Map String String)

instance Semigroup Passport where
    (Passport a) <> (Passport b) = Passport (union a b)

instance Monoid Passport where
    mempty = Passport empty

readPassport :: ReadP Passport
readPassport = fmap Passport $ singleton <$> 
    munch1 isLetter <* char ':' <*> munch1 poundAlphaNum

instance Read Passport where
    readsPrec _ = readP_to_S readPassport

data Height = Height Int String

parseHeight :: ReadP Height
parseHeight = Height <$> (read <$> munch1 isDigit) <*> munch1 isLetter

instance Read Height where
    readsPrec _ = readP_to_S parseHeight

valHeight :: Maybe Height -> Bool
valHeight (Just hgt) = case hgt of
    (Height mgn "in") -> inBetween 59 76 mgn
    (Height mgn "cm") -> inBetween 150 193 mgn
valHeight _ = False

isComplete :: Passport -> Bool
isComplete (Passport p) = all (`member` p) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: Passport -> Bool
isValid (Passport p) = and [isComplete (Passport p),
    isDigits (p ! "byr"), (inBetween 1920 2002 . read) (p ! "byr"),
    isDigits (p ! "iyr"), (inBetween 2010 2020 . read) (p ! "iyr"),
    isDigits (p ! "eyr"), (inBetween 2020 2030 . read) (p ! "eyr"),
    eye (p ! "ecl"), hair (p ! "hcl"), (valHeight . readMaybe) (p ! "hgt"), 
    ((== 9) . length) (p ! "pid"), isDigits (p ! "pid")] where 
        isDigits = and . map isDigit
        eye = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
        hair = and . (<*>) [(== '#') . head, (== 6) . length . tail,
            and . map isAlphaNum . tail] . pure

main :: IO ()
main = do
    input <- splitOn "\n\n" <$> readFile "Input/Day4Input.txt"
    let showNice pred = print . length . filter pred . map (mconcat . map read . words) $ input
    showNice isComplete
    showNice isValid