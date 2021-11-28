--Лабораторна робота 3
--студентки групи КН-31 підгрупи 1
--Попової Єлизавети
--Варіант 10

-- Мета: 

-- 1.10 Видалити кожен n-й елемент списку, напр. при n=2: "1234590"⇒"1350".

-- areTheySame :: Int -> Int-> [Int]
-- areTheySame x y | x == y = []
--                 | otherwise = [y]

-- -- tailLen :: [a] -> Int

-- removeItem :: Int -> [Int] -> [Int]
-- removeItem _ [] = []
-- removeItem x l@(y:ys) | length l < x   = l --если список меньше номера удаления
--                       | otherwise         = areTheySame x y ++ removeItem x yдs   


-- removeItem :: Int -> [Int] -> [Int]
-- removeItem _ [] = []
-- removeItem x (y:ys) | length ys < x     = areTheySame x y ++ ys
--                     | otherwise         = areTheySame x y ++ removeItem x ys   
-- 2.10 Знайти усi простi числа в указаному дiапазонi.

-- findSimple :: Int -> Int -> [Int]
-- findSimple x y n 


f :: Int -> Bool
f 1 = False
f 2 = True
f n 
    | length[x | x <- [2 .. n-1], mod n x == 0] > 0 = False
    | otherwise = True

divis :: Int -> [Int]
divis n = [x | x <- [1..n], f x, mod n x == 0]
-- divis_2 15 -> [3,5]
-- divis_2 49 -> [7]


find :: Int -> Int -> [Int]
find x y | x == y            = []
         | length (divis (x)) >= 2     = [] ++  find (x+1) y
         | otherwise         = x : find (x+1) y