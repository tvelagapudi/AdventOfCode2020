import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isLetter)
import qualified Data.Map.Strict as Map

isCommaAlpha :: Char -> Bool
isCommaAlpha = or . (<*>) [(== ','), isAlphaNum, (== ' ')] . pure

data BagRules = Bag (Map.Map String [(Int, String)])

parseName :: ReadP String
parseName = (++) <$> munch1 isLetter <* skipSpaces <*> munch1 isLetter <* skipSpaces

parseBag :: ReadP BagRules
parseBag = fmap Bag $ Map.singleton <$> parseName <* skipSpaces <* string "bags contain" 
    <*> (parseColors <$> munch1 isCommaAlpha) <* char '.'

parseNotBag :: ReadP BagRules
parseNotBag = fmap Bag $ (`Map.singleton` []) <$> parseName <* string "bags contain no other bags."

parseColors :: String -> [(Int, String)]
parseColors str = zip (map (read . (!! 1)) parsed) (map (concat . drop 2) parsed) where 
    parsed = map (init . splitOn " ") . splitOn "," $ str

instance Read BagRules where
    readsPrec _ = readP_to_S $ parseNotBag <++ parseBag

instance Semigroup BagRules where
    (Bag a) <> (Bag b) = Bag (Map.union a b)

instance Monoid BagRules where
    mempty = Bag Map.empty

(\/) :: BagRules -> String -> [(Int, String)]
(\/) (Bag b) = (b Map.!)

niceKeys :: BagRules -> [String]
niceKeys (Bag b) = Map.keys b

doesContain :: BagRules -> String -> String-> Bool
doesContain bag color = or . map (or . (<*>) [(== color), doesContain bag color] . pure . snd) . (bag \/)

numContain :: BagRules -> String -> Int
numContain bag = sum . map (mul . fmap ((+ 1) . numContain bag)) . (bag \/) where
    mul (a, c) = a * c

main :: IO ()
main = do
    input <- mconcat . map read <$> lines <$> readFile "Input/Day7Input.txt"
    print . length . filter (== True) . map (doesContain input "shinygold") . niceKeys $ input
    print . flip numContain "shinygold" $ input