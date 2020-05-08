module Test where
import Prelude hiding (length, (++), null, last,
                       init, reverse, take, drop,
                       splitAt, (!!), filter)

-- 1
const42 :: a -> Int
const42 = const 42

-- 2
factorial :: Integer -> Integer
factorial n | n >= 0    = helper 1 n
            | otherwise = error "arg must be >= 0"

      where helper acc 0 = acc
            helper acc n = helper (acc * n) (n - 1)

lst = 5 : 3 : []

-- 3
second :: [a] -> a
second = head . tail

second' :: [a] -> a
second' (_ : xs) = head xs

second'' :: [a] -> a
second'' (_ : x : _) = x

-- 4
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

null :: [a] -> Bool
null [] = True
null _ = False

-- 5
last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs

init :: [a] -> [a]
init [] = error "empty list"
init [_] = []
init (x:xs) = x : init xs

reverse :: [a] -> [a]
reverse l = rev l [] where
      rev []     a = a
      rev (x:xs) a = rev xs (x:a)

-- 6
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

xs     !! n | n < 0 = error "negative index"
[]     !! _         = error "index too large"
(x:_)  !! 0         = x
(_:xs) !! n         = xs !! (n-1)

-- 7
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
      | p x = x : filter p xs
      | otherwise = filter p xs
