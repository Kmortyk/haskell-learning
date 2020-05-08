import System.Environment
import System.IO
import Data.List


-- 1. Сумма двух чисел, считанных с клавиатуры
--
summ :: IO Integer
summ = do
         x <- readLn :: IO Integer
         y <- readLn :: IO Integer
         return (x + y)


-- 2. Печать аргументов командной строки
--
-- Пример вызова:
-- :run args a b c
--
args :: IO ()
args = do
          a <- getArgs
          mapM_ putStrLn a


-- 3. Печать содержимого файла
--
-- Пример вызова:
-- cat "lab5.hs"
--
cat :: String -> IO String
cat name = do
             handle <- openFile name ReadMode
             contents <- hGetContents handle
             return contents


-- 4. Чтение n первых строк из файла
--
-- Пример вызова:
-- fhead 10 "lab5.hs"
--
fhead :: Int -> String -> IO ()
fhead n name = do
                content <- readFile name
                let ls = lines content
                mapM_ putStrLn (take n ls)


-- Лабораторная 1 со считыванием параметров ----------------------------------------------------------------------------

-- Найти максимум из трёх чисел
--
max3 :: IO Integer
max3 = do
          x <- readLn :: IO Integer
          y <- readLn :: IO Integer
          z <- readLn :: IO Integer
          putStr "Maximum is: "
          return (max (max x y) z)


-- Найти миниум из трёх чисел
--
min3 :: IO Integer
min3 = do
          x <- readLn :: IO Integer
          y <- readLn :: IO Integer
          z <- readLn :: IO Integer
          putStr "Minimum is: "
          return (min (min x y) z)


-- Сортировка двух чисел в кортеж
--
sort2 :: IO (Integer, Integer)
sort2 = do
          x <- readLn :: IO Integer
          y <- readLn :: IO Integer
          if x < y then return (x, y)
          else return (y, x)


-- Приведение Bool к Integer
--
b2i :: Bool -> Integer
b2i True = 1
b2i False = 0

-- True - если оба операнда True, иначе False
--
bothTrue :: IO Bool
bothTrue = do
             b1 <- readLn :: IO Bool
             b2 <- readLn :: IO Bool
             return (b2i(b1) + b2i(b2) == 2)


-- Решить уравнение ax + b = 0
--
solve2 :: IO (Bool, Double)
solve2 = do
             a <- readLn :: IO Double
             b <- readLn :: IO Double
             if a == 0 then return (False, 0.0)
             else return (True, - b/a)


-- Параллельны ли два отрезка
--
isParallel :: IO Bool
isParallel = do
                (x1, y1) <- readLn :: IO (Double, Double)
                (x2, y2) <- readLn :: IO (Double, Double)
                (x3, y3) <- readLn :: IO (Double, Double)
                (x4, y4) <- readLn :: IO (Double, Double)
                return ((y2-y1)*(x4-x3) == (x2-x1)*(y4-y3))


-- Находится ли одна окружность в другой
--
isIncluded :: IO Bool
isIncluded = do
                (x1, y1) <- readLn :: IO (Double, Double)
                r1 <- readLn :: IO Double
                (x2, y2) <- readLn :: IO (Double, Double)
                r2 <- readLn :: IO Double
                let dst = sqrt((x1 - x2)^2 + (y1 - y2)^2)
                let r = r1 - r2
                return (dst <= r)


-- Проверить, является ли треугольник прямоугольным
--
isRectangular :: IO Bool
isRectangular = do
                   (x1, y1) <- readLn :: IO (Double, Double)
                   (x2, y2) <- readLn :: IO (Double, Double)
                   (x3, y3) <- readLn :: IO (Double, Double)
                   -- Теорема пифагора для каждой пары катетов и гипотенузы
                   let x21 = x2 - x1; y21 = y2 - y1
                   let x32 = x3 - x2; y32 = y3 - y2
                   let x31 = x3 - x1; y31 = y3 - y1

                   let tr1 = ((x21^2 + y21^2) + (x32^2 + y32^2) == (x31^2 + y31^2))
                   let tr2 = ((x32^2 + y32^2) + (x31^2 + y31^2) == (x21^2 + y21^2))
                   let tr3 = ((x31^2 + y31^2) + (x21^2 + y21^2) == (x32^2 + y32^2))

                   return (tr1 || tr2 || tr3)



-- Проверить, можно ли построить треугольник с такими длинами
--
isTriangle :: IO Bool
isTriangle = do
                a <- readLn :: IO Double
                b <- readLn :: IO Double
                c <- readLn :: IO Double
                return (a + b > c && b + c > a && a + c > b)


-- Упорядочены ли числа по возрастанию или убыванию
--
isSorted :: IO Bool
isSorted = do
              x <- readLn :: IO Double
              y <- readLn :: IO Double
              z <- readLn :: IO Double
              let gr = x > y && y > z
              let lr = x < y && y < z
              return (gr || lr)
