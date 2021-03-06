-- 1. --- Списки -------------------------------------------------------------------------------------------------------

-- 1) Натуральный ряд
nat :: Integer -> [Integer]
nat 1 = [1]
nat n = nat (n - 1) ++ [n]

-- 2) Нечётные натуральные числа
natdd :: Integer -> [Integer]
natdd x
    | x <= 0 = []
    | x == 1 = [1]
    | otherwise = natdd (x - 1) ++ [2*x - 1]

-- 3) Чётные натуральные числа
naten :: Integer -> [Integer]
naten x
    | x <= 0 = []
    | x == 1 = [2]
    | otherwise = naten (x - 1) ++ [2*x]

-- 4) Квадраты натуральных чисел
natsq :: Integer -> [Integer]
natsq x
    | x <= 0 = []
    | x == 1 = [1]
    | otherwise = natsq (x - 1) ++ [x*x]

-- Факториал числа
fact 0 = 1
fact n = n * fact (n - 1)

-- 5) Список факториалов
facts :: Integer -> [Integer]
facts x
    | x == 0 = [1]
    | otherwise = facts (x - 1) ++ [fact (x)]

-- 6) Список степеней двойки
twos :: Integer -> [Integer]
twos x
    | x == 0 = [1]
    | otherwise = let l = twos (x - 1)
                  in  l ++ [last (l) * 2]

-- 7) Список треугольных чисел
triags :: Integer -> [Integer]
triags x
    | x == 1 = [1]
    | otherwise = let l = triags (x - 1)
                  in  l ++ [last (l) + x]

-- 8) Треугольное число
triagn :: Integer -> Integer
triagn n
    | n == 1 = 1
    | otherwise = triagn(n - 1) + n

-- 9) Список пирамидальных чисел
pyramids :: Integer -> [Integer]
pyramids x
    | x == 1 = [1]
    | otherwise = let l = pyramids (x - 1)
                  in  l ++ [last(l) + triagn(x)]

-- 2. --- Функции ------------------------------------------------------------------------------------------------------

-- 1) Арифметическое среднее
mean :: [Double] -> Double
mean [] = 0
mean xs = summ xs / (fromIntegral $ length xs)

-- 2) N - ый элемент списка
getn :: Integer -> [a] -> a
getn n (x:xs)  | n == 1 = x
               | otherwise = getn (n - 1) xs

-- 3) Сложение двух списков
suml :: [Integer] -> [Integer] -> [Integer]
suml [] _ = []
suml _ [] = []
suml (x:xs) (y:ys) = x + y : (suml xs ys)

-- 4) Перестановка чётных и нечётных
rear :: [Integer] -> [Integer]
rear [] = []
rear [a] = [a]
rear (x:y:xs)
              | odd x && even y = y : x : rear xs
              | even x && odd y = y : x : rear xs
              | otherwise = x : y : rear xs

-- 5) 2 в степени n
twopow :: Integer -> Integer
twopow n
        | n == 0 = 1
        | even n = twopow k * twopow k
        | odd n  = twopow k * twopow k * 2
        where k = n `div` 2

-- 6) Удаление из списка всех нечётных чисел
removeOdd :: [Integer] -> [Integer]
removeOdd [] = []
removeOdd (x:xs)
                 | odd x = removeOdd xs
                 | otherwise = x : removeOdd xs

-- 7) Удаление из списка пустых строк
removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs)
                    | length x == 0 = removeEmpty xs
                    | otherwise = x : removeEmpty xs

-- 8) Количество элементов списка, равных True
countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue (x:xs)
                  | x == True = 1 + countTrue xs
                  | otherwise = countTrue xs

-- 9) Поменять знак всех отрицательных элементов списка
makePositive :: [Integer] -> [Integer]
makePositive [] = []
makePositive (x:xs)
                    | x < 0     = - x : makePositive xs
                    | otherwise = x : makePositive xs

-- 10) Удалить все вхождения символа из строки
delete :: Char -> String -> String
delete c [] = []
delete c (x:xs)
                 | x == c    = delete c xs
                 | otherwise = x : delete c xs

-- 11) Заменить все вхождения символа на заданный
substitute :: Char -> Char -> String -> String
substitute c r [] = []
substitute c r (x:xs)
                 | x == c    = r : substitute c r xs
                 | otherwise = x : substitute c r xs