type Direction    = (Char, Double)
type Position     = (Double, Double, Double)
type WayPosition  = (Double, Double, Double, Double)

parse :: String -> Direction
parse (c:cs) = (c, read cs)

doDirection :: Position -> Direction -> Position
doDirection (x,y,d) dir = case dir of
    ('N', a) -> (x, y + a, d)
    ('S', a) -> (x, y - a, d)
    ('E', a) -> (x + a, y, d)
    ('W', a) -> (x - a, y, d)
    ('L', a) -> (x, y, d + a)
    ('R', a) -> (x, y, d - a)
    ('F', a) -> (x + a * cos (d * pi/180) , y + a * sin (d * pi/180) , d) where

doWayDirection :: WayPosition -> Direction -> WayPosition
doWayDirection (x,y,w,p) dir = case dir of
    ('N', a) -> (x, y, w, p + a)
    ('S', a) -> (x, y, w, p - a)
    ('E', a) -> (x, y, w + a, p)
    ('W', a) -> (x, y, w - a, p)
    ('L', a) -> (x, y, w * cos (a * pi/180) - p * sin (a * pi/180), 
        p * cos (a * pi/180) + w * sin (a * pi/180))
    ('R', a) -> (x, y, w * cos (-a * pi/180) - p * sin (-a * pi/180), 
        p * cos (-a * pi/180) + w * sin (-a * pi/180))
    ('F', a) -> (x + a * w, y + a * p, w, p)

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "Input/Day12Input.txt"
    print . round . (\(x,y,_)   -> abs x + abs y) . foldl doDirection (0,0,0) $ input
    print . round . (\(x,y,_,_) -> abs x + abs y) . foldl doWayDirection (0,0,10,1) $ input