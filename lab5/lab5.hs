--Лабораторна робота 5
--студентки групи КН-31 підгрупи 1
--Попової Єлизавети
--Варіант 10

-- Мета: Ознайомитись з модульною органiзацiєю програм та засобами введення-
-- виведення. Набути досвiду компiляцiї Haskell-програм.

--Інструкція

-- isDropEveryConsoleToConsole --ввод з консолі, вивід у консоль функції 1

-- Приклад
-- isDropEveryConsoleToConsole
-- Enter int
-- 2
-- Enter string
-- "1234590"
-- Result:
-- "1350"

--isDropEveryConsoleToFile --ввод з консолі, вивід у файл "outputDropEvery.txt" функції 1

--isDropEveryFileToConsole --ввод з файлу "inputDropEvery.txt", вивід у консоль

--isDropEveryFileToFile --ввод з файлу "inputDropEvery.txt", вивід у файл "outputDropEvery.txt"


import System.IO

--Видалити кожен n-й елемент списку, напр. при n=2: "1234590"⇒"1350"

dropEvery _ [] = []
dropEvery 0 xs = xs
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)


isDropEveryConsoleToConsole::IO()
isDropEveryConsoleToConsole = do
    putStr "Enter int\n"
    line <- getLine
    let n = read line :: Int
    putStr "Enter string\n"
    line <- getLine
    let arr = read line :: [Char]
    putStr "Result:\n"
    print (dropEvery n arr)


isDropEveryConsoleToFile::IO()
isDropEveryConsoleToFile = do
    out <- openFile "outputDropEvery.txt" WriteMode
    putStr "Enter int\n"
    line <- getLine
    let n = read line :: Int
    hPutStr out (line ++ "\n")
    putStr "Enter string\n"
    line <- getLine
    let arr = read line :: [Char]
    hPutStr out (line ++ "\n")
    hPutStr out "Result:\n"
    hPrint out (dropEvery n arr)
    hClose out

 

isDropEveryFileToConsole::IO()
isDropEveryFileToConsole = do
    inp <- openFile "inputDropEvery.txt" ReadMode
    line <- hGetLine inp
    let n = read line :: Int
    line <- hGetLine inp
    putStr (line ++ "\n")
    putStr "Result:\n"
    print (dropEvery n line)
    hClose inp




isDropEveryFileToFile::IO()
isDropEveryFileToFile = do
    inp <- openFile "inputDropEvery.txt" ReadMode
    out <- openFile "outputDropEvery.txt" WriteMode
    line <- hGetLine inp
    let n = read line :: Int
    hPutStr out (line ++ "\n")
    line <- hGetLine inp
    hPutStr out (line ++ "\n")
    hPutStr out "Result:\n"
    hPrint out (dropEvery n line)
    hClose inp
    hClose out


