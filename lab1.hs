-- 1. --- Типы ---------------------------------------------------------------------------------------------------------

-- 1)  (('a',1), "abc", [1.0,2.0])                                                           -> ((Char,Integer), String, [Double])
-- 2)  [(1.0,True,("abc",1)),(2.0,False,("bcd",1))]                                          -> [(Double,Bool,(String,Integer))]
-- 3)  ([1,2],[1.0,2.0],[(True,'a'),(False,'b')])                                            -> ([Integer],[Double],[(Bool,Char)])
-- 4)  [[[(1,True),(2,True)],[(1,True),(2,True)]],[[(1,True),(2,True)],[(1,True),(2,True)]]] -> [[[(Integer,Bool)]]]
-- 5)  ((('a','b'),'c'),["abc","def"])                                                       -> (((Char,Char),Char),[String])
-- 6)  (([2.0,3.0],[True,False]),[1,2,4,5])                                                  -> (([Double],[Bool]),[Integer])
-- 7)  [1, (2,[True,False])] - некорректное задание ---------------------------------------- -> [Integer, (Integer,[Bool])]
-- 8)  (True,([False,True],[1,1,2]))                                                         -> (Bool,([Bool],[Integer]))
-- 9)  [([False,True,True],[2.0,1.0,3.52]), ([True],[45.0,11.5])]                            -> [([Bool],[Double])]
-- 10) [([1,1,1],['a','b','c']),([1,2,3],['a','b']),([1,3,3],['a','b','c','d'])]             -> [([Integer],[Char])]

-- 2. --- Функции ------------------------------------------------------------------------------------------------------

-- 1) Найти максимум из трёх чисел
max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) z

-- 2) Найти миниум из трёх чисел
min3 :: Integer -> Integer -> Integer -> Integer
min3 x y z = min (min x y) z

-- 3) Сортировка двух чисел в кортеж
sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 x y
    | x < y = (x, y)
    | otherwise = (y, x)

-- Приведение Bool к Integer
boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

-- 4) True - если оба операнда True, иначе False
bothTrue :: Bool -> Bool -> Bool
bothTrue x y
    | boolToInt x + boolToInt y == 2 = True
    | otherwise = False    

-- 5) Решить уравнение ax + b = 0
solve2 :: Double -> Double -> (Bool, Double)
solve2 a b
    | a == 0    = (False, 0.0)
    | otherwise = (True, - b/a)

-- 6) Параллельны ли два отрезка
isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (x1,y1) (x2,y2) (x3,y3) (x4,y4) = (y2-y1)*(x4-x3) == (x2-x1)*(y4-y3)   

-- 7) Находится ли одна окружность в другой
isIncluded :: (Double, Double) -> Double -> (Double, Double) -> Double -> Bool
isIncluded (x1,y1) r1 (x2,y2) r2 = sqrt((x1 - x2)^2 + (y1 - y2)^2) <= r1 - r2   

-- 8) Проверить, является ли треугольник прямоугольным
isRectangular :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
isRectangular (x1,y1) (x2,y2) (x3,y3) -- Теорема пифагора для каждой пары кактетов и гипотенузы
    | ((x2-x1)^2 + (y2-y1)^2) + ((x3-x2)^2 + (y3-y2)^2) == ((x3-x1)^2 + (y3-y1)^2) = True
    | ((x3-x2)^2 + (y3-y2)^2) + ((x3-x1)^2 + (y3-y1)^2) == ((x2-x1)^2 + (y2-y1)^2) = True
    | ((x3-x1)^2 + (y3-y1)^2) + ((x2-x1)^2 + (y2-y1)^2) == ((x3-x2)^2 + (y3-y2)^2) = True
    | otherwise = False

-- 9) Проверить, можно ли построить треугольник с такими длинами
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c -- Сумма двух любых сторон больше третьей
    | a + b > c && b + c > a && a + c > b = True
    | otherwise = False

-- 10) Упорядочены ли числа по возрастанию или убыванию
isSorted :: Double -> Double -> Double -> Bool
isSorted x y z
    | x < y && y < z = True
    | x > y && y > z = True
    | otherwise = False    