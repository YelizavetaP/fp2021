--Лабораторна робота 2
--студентки групи КН-31 підгрупи 1
--Попової Єлизавети
--Варіант 11

--Мета: Набути досвiду визнач ення рекурсивних функцiй, використання механiзму 
--      зiставлення зi зразком i роботи з кортежами та списками.

-- :load lab2\\lab2.hs

-- Визначте вказанi функцiї в кожному з завдань: 
-- 1. Повторити n-кратно кожен елемент списку, напр. при n=2: "asd"⇒ [’a’, ’a’, ’s’, ’s’, ’d’, ’d’].

-- а) без застосування вбудованих функцiй

repeatNTimes :: Int -> a -> [a]
repeatNTimes 0 _ = []
repeatNTimes n x = x : repeatNTimes (n - 1) x

func1 :: Int -> [a] -> [a]
func1 _ [] = []
func1 n (x:xs) = repeatNTimes n x ++ func1 n xs
--func1 2 [1,2,3] => [1,1,2,2,3,3]


--б) з застосуванням вбудованих функцiй
func2:: Int -> [a]->[a]
func2 _ [] =[]
func2 n (x:xs) = replicate n x  ++ func2 n xs
--func2 3 ["a","b","c"] => ["a","a","a","b","b","b","c","c","c"]


-- 2. Обчислити функцiю Ейлера φ(m)
--Функція рахує к-сть чисел, взаємно простих з n, тобто для яких НСД з n є 1

-- а) без застосування вбудованих функцiй

-- НСД
gcd' :: Integer -> Integer -> Integer
gcd' n m | (m > n) = gcd' m n 
         | (m == 0) = n
         | otherwise = gcd' m (n `rem` m)
 

-- чи дорівнює НСД 1
mutual :: Integer -> Integer -> Integer 
mutual x y = if ((gcd' x y)==1) then 1 else 0


-- перевіряємо кожне число, починаючи з с на відповідність умові,
-- якщо виконується додаємо 1 до результату та переходимо до наступного числа, поки с не стане рівне n

-- так як шукаємо кількість чисел на проміжку від 1 до n, c має бути завжди 1 при вводі
func3 :: Integer -> Integer -> Integer
func3 n c 
    | n==1 = 1
    | n==c = 0
    | otherwise = 0 + mutual n c + func3 n (c+1)
--func3 1 1 =>1
--func3 5 1 =>4 



-- б) з застосуванням вбудованих функцiй
-- використовуємо вбудовану ункцію взначення НСД
mutual_2 :: Integer -> Integer -> Integer 
mutual_2 x y = if ((gcd x y)==1) then 1 else 0

func4 :: Integer -> Integer -> Integer
func4 n c 
    | n==1 = 1
    | n==c = 0
    | otherwise = 0 + mutual_2 n c + func3 n (c+1)


-- -- func4 1 1 =>1
-- -- func4 5 1 =>4 