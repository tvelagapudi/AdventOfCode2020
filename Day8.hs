import Data.List (findIndices)

data Action = Action String Int

parse :: String -> Action
parse str = Action (fst ls) (intConvert . tail . snd $ ls) where 
    ls = splitAt 3 str

intConvert :: String -> Int
intConvert str = case head str of
    '+' -> read . tail $ str
    '-' -> read str

isDone :: Int -> Int -> [Int] -> [Action] -> (Int, Bool)
isDone index val done actions
    | index >= length actions - 1 = (val, True)
    | index `elem` done           = (val, False)
    | otherwise = case actions !! index of
        Action "acc" num -> isDone (index + 1)  (val + num) (index : done) actions
        Action "jmp" num -> isDone (index + num) val        (index : done) actions
        Action "nop" _   -> isDone (index + 1)   val        (index : done) actions

alterAction :: Action -> Action
alterAction (Action "nop" a) = Action "jmp" a
alterAction (Action "jmp" a) = Action "nop" a
alterAction a = a

changeActions :: [Action] -> Int -> [Action]
changeActions actions index = fst spl ++ [alterAction (head . snd $ spl)] ++ (tail . snd $ spl) where
    spl = splitAt index actions

tryAlter :: [Action] -> [[Action]]
tryAlter actions = map (changeActions actions) (findIndices isNotAcc actions) where
    isNotAcc (Action "acc" _) = False
    isNotAcc _ = True

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "Input/Day8Input.txt"
    print . fst . isDone 0 0 [] $ input
    print . fst . head . filter ((== True) . snd) . map (isDone 0 0 []) . tryAlter $ input