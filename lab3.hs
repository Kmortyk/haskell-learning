-- 7. Операции над строками --------------------------------------------------------------------------------------------

-- Удаление всех символов из строки
clr :: String -> String
clr str = []

-- Удаление всех вхождений указанного символа
del :: Char -> String -> String
del c [] = []
del c (x:xs)
             | x == c    = del c xs
             | otherwise = x : del c xs

-- Замена всех вхождений одного символа на другой
rep :: Char -> Char -> String -> String
rep c r [] = []
rep c r (x:xs)
             | x == c    = r : rep c r xs
             | otherwise = x : rep c r xs

-- Добавление в начало строки указанного символа
app :: Char -> String -> String
app c str = c : str

-- Набор действий над строками
data Op = Clear
        | Delete Char
        | Append Char
        | Replace Char Char


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
processAll str [] = str
processAll str (o:ops) = processAll (process str o) ops


-- Удаление из второй строки всех символов первой
--
-- Пример использования:
-- deleteAll "abco" "Hellob mya friendc, how there?"
--
deleteAllOps :: String -> String -> [Op] -> String
deleteAllOps [] str2 ops = processAll str2 ops
deleteAllOps (x:xs) str2 ops = deleteAllOps xs str2 ((Delete x) : ops)

deleteAll :: String -> String -> String
deleteAll str1 str2 = deleteAllOps str1 str2 []
