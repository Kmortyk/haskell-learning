-- Функции высшего порядка ---------------------------------------------------------------------------------------------

import Data.List -- импорт для foldl'

-- 1. Арифметическое среднее элементов списка
--
meant :: [Integer] -> Double
summt = foldl (+) 0
meant xs =
           let s = summt xs; n = length xs
           in fromIntegral s / fromIntegral n


-- 1. Арифметическое среднее элементов списка (в один проход)
--
mean :: [Integer] -> Double
mean xs = divf $ foldl' sumf (0, 0) xs
  where
    sumf (s, n) x = (s+x, n+1)
    divf (s, n) = fromIntegral s / fromIntegral n


-- 2. Скалярное произведение двух списков
--
dot xs ys = foldr (+) 0 (zipWith (*) xs ys)


-- 3. Количество четных элементов в списке
--
evenn :: Integer -> Integer -> Integer
evenn x y
      | even x = y + 1
      | otherwise = y

countEven :: [Integer] -> Integer
countEven xs = foldr evenn 0 xs


-- 4. `Быстрая` сортировка
--
quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (x:xs) =
  let as = quicksort (filter (\y -> y < x) xs)
      bs = quicksort (filter (\y -> y >= x) xs)
  in  as ++ [x] ++ bs


-- 5. `Быстрая` сортировка обобщённая
--
-- Примеры использования:
-- qsort (\y x -> y < x) [2,1,10,8,9,7]
-- qsort (\y x -> y > x) [2,1,10,8,9,7]
--
qsort :: (Integer -> Integer -> Bool) -> [Integer] -> [Integer]
qsort cmp [] = []
qsort cmp (x:xs) =
  let as = qsort cmp (filter (\y -> (cmp y x)) xs)
      bs = qsort cmp (filter (\y -> not(cmp y x)) xs)
  in  as ++ [x] ++ bs

-- Лабораторная 3 в функциональном стиле -------------------------------------------------------------------------------

-- Удаление всех символов из строки
--
clr str = []

-- Удаление всех вхождений указанного символа
--
del c = filter (\y -> y /= c)

-- Замена всех вхождений одного символа на другой
--
rep c r = map (\y -> if (y == c) then r else y)

-- Добавление в начало строки указанного символа
--
app c str = c : str

-- Набор действий над строками
data Op = Clear | Delete Char | Append Char | Replace Char Char

-- Выполнить некоторое действие над строкой
--
-- Примеры использования:
-- process "Abc" (Delete 'A')
-- process "Abc" (Append 'A')
-- process "Abc" (Replace 'A' 'b')
--
process :: String -> Op -> String
process str op = case op of
            Clear         -> clr str
            (Delete c)    -> del c str
            (Append c)    -> app c str
            (Replace c r) -> rep c r str


-- Выполнить набор действий над строкой
--
-- Пример использования:
-- processAll "Hello, World!" [(Replace 'o' 'a'), (Append 'f'), (Replace 'f' 'g')]
--
processAll :: String -> [Op] -> String
processAll = foldl (\str op -> process str op)


-- Удаление из второй строки всех символов первой
--
-- Пример использования:
-- deleteAll "abco" "Hellob mya friendc, how there?"
--
deleteAll :: String -> String -> String
deleteAll str1 = filter (\y -> not(y `elem` str1))

