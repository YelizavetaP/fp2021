--Лабораторна робота 3
--студентки групи КН-31 підгрупи 1
--Попової Єлизавети
--Варіант 10

-- Мета: 

-- 1.10 Видалити кожен n-й елемент списку, напр. при n=2: "1234590"⇒"1350".

--со встроеніми
dropEvery _ [] = []
dropEvery 0 xs = xs
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

-- dropEvery 2 "1234590" => "1350"
-- dropEvery 3 "1234590" => "12450"

-- без встроеніх
myDrop 0 x = x
myDrop n (x:xs) = myDrop (n-1) xs
myDrop _ _ = []

myTake ::  Int -> [b] -> [b]
myTake 0 l = []
myTake n (l:ls) = l :  myTake (n-1) ls

dropEvery2 _ [] = []
dropEvery2 0 xs = xs
dropEvery2 n xs = myTake (n-1) xs ++ dropEvery n (myDrop n xs)

-- dropEvery 2 "1234590" => "1350"
-- dropEvery 3 "1234590" => "12450"

-- 2.10 Знайти усi простi числа в указаному дiапазонi.

-- без вбудованих функцій

f :: Int -> Bool
f 1 = False
f 2 = True
f n 
    | length[x | x <- [2 .. n-1], mod n x == 0] > 0 = False
    | otherwise = True


find1 :: Int -> Int -> [Int]
find1 x y | x > y            = []
          | f(x)            = x : find1 (x+1) y
         | otherwise        = find1 (x+1) y

-- find1 1 5 => [2,3,5]

-- з вбудованими функціями
prime 1 = False
prime n = (product[1..(n-1)]+1) `mod` n ==0

find2 :: Int -> Int -> [Int]
find2 x y | x > y            = []
          | x<=0             = find2 1 y
          | prime(x)            = x : find2 (x+1) y
         | otherwise        = find2 (x+1) y

-- find2 1 5 => [2,3,5]