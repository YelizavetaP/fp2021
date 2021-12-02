{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--Лабораторна робота №4
--студентки групи КН-31 підгрупа 1
--Попова Єлизавета
--Варіант 10

--Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення нових типiв та класiв типiв i їх використання.

--Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути книгою (автор/спiвавтори, назва, мiсто, видавництво, рiк), 
--статтею (автор/спiвавтори, назва статтi, назва журналу, рiк, номер журналу, сторiнки) або 
--тезами доповiдi (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). Визначне функцiї для :

--статистика публiкацiй автора 

data Book = Book {
bookAuthor :: String,
bookName :: String,
bookCity :: String,
publishingHouse :: String,
bookYear :: Int
} deriving (Eq, Show)

data Article = Article {
articleAuthor :: String,
articleName :: String,
magazine :: String,
articleYear :: Int,
magazineNumber :: Int,
pageCount :: Int
} deriving (Eq, Show)

data Thesis = Thesis {
thesisAuthor :: String,
thesisName :: String,
conference :: String,
thesisCity :: String,
thesisYear :: Int,
pageCountThesis:: Int
} deriving (Eq, Show)


arrBook :: [Book]
arrBook = [(Book "Leo Tolstoy" "Anna Karenina" "Moscow" "Neva" 1878),(Book "Harper Lee" "To Kill a Mockingbird" "New York" "TM" 1960),(Book "Harper Lee" "To be a Mockingbird" "New York" "TM" 1965)]
arrArticle :: [Article]
arrArticle = [(Article "Harper Lee" "How to survive" "New York Times" 1970 54 50),(Article "Sofia Recaro" "BIrd in Antarctica" "Zootrop" 2005 20 10)]

arrThesis :: [Thesis]
arrThesis = [(Thesis "Harper Lee" "How to become happy" "Live conference №15" "London" 1980 120)]


findBooks' :: String -> [Book] -> [Book]
findBooks' _ [] = []
findBooks' x (y : ys) = if x == bookAuthor y then  y : findBooks' x ys  else findBooks' x ys

findArticles' :: String -> [Article] -> [Article]
findArticles' _ [] = []
findArticles' x (y : ys) = if x == articleName y then  y : findArticles' x ys else findArticles' x ys

findThesis' :: String -> [Thesis] -> [Thesis]
findThesis' _ [] =[]
findThesis' x (y : ys) = if x == thesisAuthor y then  y : findThesis' x ys else findThesis' x ys

checkAuthor :: String -> ([Book],[Article],[Thesis])
checkAuthor a = (findBooks' a arrBook, findArticles' a arrArticle,findThesis' a arrThesis)

info :: ([Book],[Article],[Thesis]) -> [(Int, String)]
info (x,y,z) = [(length x,"Book"),(length y,"Article"),(length z,"Thesis")]

find :: String->[(Int,String)]
find x = info(checkAuthor x)
    

-- find "Harper Lee"       
-- [(2,"Book"),(1,"Article"),(1,"Thesis")]

-- find "Leo Tolstoy"    
-- [(1,"Book"),(0,"Article"),(0,"Thesis")]


--Висновок: В ході лабораторної роботи ми ознайомились з системою типiв та класiв типiв та набули досвiду визначення нових типiв та класiв типiв i їх використання.

