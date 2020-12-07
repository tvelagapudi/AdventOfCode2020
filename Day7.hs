import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isLetter)
import qualified Data.Map.Strict as Map

isCommaAlpha :: Char -> Bool
isCommaAlpha = or . (<*>) [(== ','), isAlphaNum, (== ' ')] . pure

type BagRules = Map.Map String [(Int, String)]

newtype BagRule = Rule { getRule :: BagRules }

parseName :: ReadP String
parseName = (++) <$> munch1 isLetter <* skipSpaces <*> munch1 isLetter <* skipSpaces

parseBag :: ReadP BagRule
parseBag = fmap Rule $ Map.singleton <$> parseName <* skipSpaces <* string "bags contain" 
    <*> (parseColors <$> munch1 isCommaAlpha) <* char '.'

parseNotBag :: ReadP BagRule
parseNotBag = fmap Rule $ (`Map.singleton` []) <$> parseName <* string "bags contain no other bags."

parseColors :: String -> [(Int, String)]
parseColors str = zip (map (read . (!! 1)) parsed) (map (concat . drop 2) parsed) where 
    parsed = map (init . splitOn " ") . splitOn "," $ str

instance Read BagRule where
    readsPrec _ = readP_to_S $ parseNotBag <++ parseBag

doesContain :: BagRules -> String -> String-> Bool
doesContain bag color = or . map (or . (<*>) [(== color), doesContain bag color] . pure . snd) . (bag Map.!)

numContain :: BagRules -> String -> Int
numContain bag = sum . map (mul . fmap ((+ 1) . numContain bag)) . (bag Map.!) where
    mul (a, c) = a * c

main :: IO ()
main = do
    input <- Map.unions . map (getRule . read) . lines <$> readFile "Input/Day7Input.txt"
    print . length . filter (== True) . map (doesContain input "shinygold") . Map.keys $ input
    print . flip numContain "shinygold" $ input