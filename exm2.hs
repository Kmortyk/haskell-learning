-- пример использования свёртки
-- сумма квадратов списка
--
sumsq :: [Integer] -> Integer
sumsq = foldr (\x s -> x^2 + s) 0 where


-- пример использования типа Maybe
-- решение уравнения ax + b = 0
--
eqt :: Double -> Double -> Maybe Double
eqt a b
    | a == 0    = Nothing
    | otherwise = Just (- b/a)

-- пример использования сопоставления с образцом
--
is_two :: Integer -> Bool
is_two 1 = False
is_two 2 = True
is_two _ = False

-- пример использования конструкции case... of...
--
data Weather = Sunny | Rain | Snow
tip :: Weather -> String
tip x = case x of
    Sunny -> "Remember to wear a hat."
    Rain -> "Take an umbrella."
    Snow -> "Put on the mittens."

--
-- Тип-произведение - тип данных, конструктор которого имеет аргументы.
-- Тип-произведение фактически описывает декартово произведение элементов, которые его составляют.
-- Конструктор такого типа представляет собой функцию без тела.
data Point = Point Double Double
                   deriving Show

-- пример: dst (Point 2.0 3.0)
dst :: Point -> Double
dst (Point x y) = sqrt(x^2 + y^2)