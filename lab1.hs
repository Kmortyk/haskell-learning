-- 1 -----------------------------------------------
-- 1) (('a',1), "abc", [1.0,2.0])     -> ((Char,Integer), String, [Double])
-- 2) [(1.0,True,("abc",1))]          -> [(Double,Bool,(String,Integer))]
-- 3) ([1,2],[1.0,2.0],[(True,'a')])  -> ([Integer],[Double],[(Bool,Char)])
-- 4) [[[(1,True),(2,True)]]]         -> [[[(Integer,Bool)]]]
-- 5) ((('a','b'),'c'),["abc","def"]) -> (((Char,Char),Char),[String])
-- 6) (([Double],[Bool]),[Integer])
-- 7) [Integer, (Integer,[Bool])]
-- 8) (Bool,([Bool],[Integer]))
-- 9) [([Bool],[Double])]
-- 10) [([Integer],[Char])]

-- 2 -----------------------------------------------
max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) z

min3 :: Integer -> Integer -> Integer -> Integer
min3 x y z = min (min x y) z

sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 x y
    | x < y = (x, y)
    | otherwise = (y, x)

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

bothTrue :: Bool -> Bool -> Bool
bothTrue x y
    | boolToInt x + boolToInt y == 2 = True
    | otherwise = False    

solve2 :: Double -> Double -> (Bool, Double)
solve2 a b
    | a /= 0 = (True, - a/b)
    | a == 0 = (False, 0.0)

isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (x1,y1) (x2,y2) (x3,y3) (x4,y4) = (y2-y1)*(x4-x3) == (x2-x1)*(y4-y3)   

isIncluded :: (Double, Double) -> Double -> (Double, Double) -> Double -> Bool
isIncluded (x1,y1) r1 (x2,y2) r2 = sqrt((x1 - x2)^2 + (y1 - y2)^2) <= r1 - r2   

isRectangular :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
isRectangular (x1,y1) (x2,y2) (x3,y3) 
    | ((x2-x1)^2 + (y2-y1)^2) + ((x3-x2)^2 + (y3-y2)^2) == ((x3-x1)^2 + (y3-y1)^2) = True
    | ((x3-x2)^2 + (y3-y2)^2) + ((x3-x1)^2 + (y3-y1)^2) == ((x2-x1)^2 + (y2-y1)^2) = True
    | ((x3-x1)^2 + (y3-y1)^2) + ((x2-x1)^2 + (y2-y1)^2) == ((x3-x2)^2 + (y3-y2)^2) = True
    | otherwise = False

isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c
    | a + b < c && b + c < a && a + c < b = True
    | otherwise = False

isSorted :: Double -> Double -> Double -> Bool
isSorted x y z
    | x < y && y < z = True
    | otherwise = False    