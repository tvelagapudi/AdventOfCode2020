type Direction    = (Char, Double)
type Position     = (Double, Double, Double)
type WayPosition  = (Position, Position)

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
doWayDirection ((x,y,d),(w,p,b)) dir = case dir of
    ('N', a) -> ((x, y, d), (w, p + a, b))
    ('S', a) -> ((x, y, d), (w, p - a, b))
    ('E', a) -> ((x, y, d), (w + a, p, b))
    ('W', a) -> ((x, y, d), (w - a, p, b))
    ('L', a) -> ((x, y, d), (w * cos (a * pi/180) - p * sin (a * pi/180), 
        p * cos (a * pi/180) + w * sin (a * pi/180), b))
    ('R', a) -> ((x, y, d), (w * cos (-a * pi/180) - p * sin (-a * pi/180), 
        p * cos (-a * pi/180) + w * sin (-a * pi/180), b))
    ('F', a) -> ((x + a * w, y + a * p, d), (w, p, b))

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "Input/Day12Input.txt"
    print . round . (\(x,y,_)     -> abs x + abs y) . foldl doDirection (0,0,0) $ input
    print . round . (\((x,y,_),_) -> abs x + abs y) . foldl doWayDirection ((0,0,0),(10,1,0)) $ input