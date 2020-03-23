-- 1. --- Типы ---------------------------------------------------------------------------------------------------------

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

-- 2. --- Функции ------------------------------------------------------------------------------------------------------

-- Найти максимум из трёх чисел
max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) z

-- Найти миниум из трёх чисел
min3 :: Integer -> Integer -> Integer -> Integer
min3 x y z = min (min x y) z

-- Сортировка двух чисел в кортеж
sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 x y
    | x < y = (x, y)
    | otherwise = (y, x)

-- Приведение Bool к Integer
boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

-- True - если оба операнда True, иначе False
bothTrue :: Bool -> Bool -> Bool
bothTrue x y
    | boolToInt x + boolToInt y == 2 = True
    | otherwise = False    

-- Решить уравнение ax + b = 0
solve2 :: Double -> Double -> (Bool, Double)
solve2 a b
    | a == 0    = (False, 0.0)
    | otherwise = (True, - b/a)

-- Параллельны ли два отрезка
isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (x1,y1) (x2,y2) (x3,y3) (x4,y4) = (y2-y1)*(x4-x3) == (x2-x1)*(y4-y3)   

-- Находится ли одна окружность в другой
isIncluded :: (Double, Double) -> Double -> (Double, Double) -> Double -> Bool
isIncluded (x1,y1) r1 (x2,y2) r2 = sqrt((x1 - x2)^2 + (y1 - y2)^2) <= r1 - r2   

-- Проверить, является ли треугольник прямоугольным
isRectangular :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
isRectangular (x1,y1) (x2,y2) (x3,y3) -- Теорема пифагора для каждой пары кактетов и гипотенузы
    | ((x2-x1)^2 + (y2-y1)^2) + ((x3-x2)^2 + (y3-y2)^2) == ((x3-x1)^2 + (y3-y1)^2) = True
    | ((x3-x2)^2 + (y3-y2)^2) + ((x3-x1)^2 + (y3-y1)^2) == ((x2-x1)^2 + (y2-y1)^2) = True
    | ((x3-x1)^2 + (y3-y1)^2) + ((x2-x1)^2 + (y2-y1)^2) == ((x3-x2)^2 + (y3-y2)^2) = True
    | otherwise = False

-- Проверить, можно ли построить треугольник с такими длинами
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c -- Сумма двух любых сторон больше третьей
    | a + b > c && b + c > a && a + c > b = True
    | otherwise = False

-- Упорядочены ли числа по возрастанию или убыванию
isSorted :: Double -> Double -> Double -> Bool
isSorted x y z
    | x < y && y < z = True
    | x > y && y > z = True
    | otherwise = False    