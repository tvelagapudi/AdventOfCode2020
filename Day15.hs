import Data.Map.Strict hiding (map, filter)
import Data.Maybe (fromJust)

type Record = Map Int Int

start :: Record
start = fromList $ zip [0,5,4,1,10,14,7] [1..]

addOne :: Record -> (Int, Int) -> (Record, (Int, Int))
addOne record (val, ind)
    | val `member` record = (adjust (const ind) val record, (ind - record ! val, ind + 1))
    | otherwise           = (insert val ind record, (0, ind + 1))

addMult :: Int -> (Record, (Int, Int))
addMult 8 = addOne start (0, 8)
addMult ind = uncurry addOne $ addMult (ind - 1)

main :: IO ()
main = do
    let lst = fst . snd $ addMult 2019
        lst'= fst . snd $ addMult 29999999
    print lst
    print lst'
