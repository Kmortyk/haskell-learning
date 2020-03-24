-- 7. Ряд целых чисел --------------------------------------------------------------------------------------------------

data Number = Zero | Next Number
              deriving Show


-- Преобразование
fromInt :: Integer -> Number
fromInt x
        | x <= 0 = Zero
        | otherwise = Next (fromInt (x - 1))


-- Обратное преобразование
--
-- Пример использования:
-- toInt (Next (Next (Next (Next (Next (Next (Next (Next (Next (Next Zero))))))))))
--
toInt :: Number -> Integer
toInt Zero = 0
toInt (Next num) = 1 + toInt num


-- Сложение
--
-- Пример использования:
-- plus (Next(Zero)) (Next(Next(Zero)))
--
plus :: Number -> Number -> Number
plus Zero Zero = Zero
plus (Next num) Zero = Next num
plus Zero (Next num) = Next num
plus (Next num1) (Next num2) = Next(Next(plus num1 num2))


-- Умножение
--
-- Пример использования:
-- mult (Next(Next(Next(Zero)))) (Next(Next(Zero)))
-- mult (Next(Next(Next(Zero)))) (Next(Next(Next(Zero))))
--
mult :: Number -> Number -> Number
-- Zero --------------------------
mult Zero Zero = Zero
mult (Next num) Zero = Zero
mult Zero (Next num) = Zero
-- One ---------------------------
mult (Next Zero) (Next num) = Next num
mult (Next num) (Next Zero) = Next num
-- Common ------------------------
mult (Next num1) (Next num2) = plus (Next num1) (mult (Next num1) num2)


-- Уменьшение на единицу
--
-- Примеры использования:
-- dec (Next(Next(Next(Zero))))
-- dec (Next (Next Zero))
-- dec (Next Zero)
-- dec Zero
--
dec :: Number -> Number
dec Zero = Zero
dec (Next num) = num


-- Вычисление факториала
--
-- Пример использования:
-- fact (Next(Next(Next(Zero))))
-- fact (Next (Next (Next (Next (Next (Next Zero))))))
--
fact :: Number -> Number
fact Zero = Next Zero
fact (Next num) = mult (Next num) (fact (num))