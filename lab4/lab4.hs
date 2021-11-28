--Лабораторна робота 4
--студентки групи КН-31 підгрупи 1
--Попової Єлизавети
--Варіант 10

-- :load lab4\\lab4.hs


-- Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути книгою (ав-
-- тор/спiвавтори, назва, мiсто, видавництво, рiк), статтею (автор/спiвавтори, на-
-- зва статтi, назва журналу, рiк, номер журналу, сторiнки) або тезами доповiдi
-- (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). Ви-
-- значне функцiї для :

-- статистика публiкацiй автора — кiлькiсть, обсяг (у сторiнках), тип;

data Book = Book {
    authorBook :: String,
    nameBook :: String,
    cityBook :: String,
    published :: String,
    yearBook :: Int
} deriving (Eq, Show)

data Article = Article {
    authorArticle :: String,
    nameArticle :: String,
    magazineName :: String,
    yearArticle :: Int,
    magazineNumber :: Int,
    pageNumber :: Int
} deriving (Eq, Show)

data Thesis = Thesis {
    authorThesis :: String,
    nameThesis :: String,
    nameConference :: String,
    cityThesis :: String,
    yearThesis :: Int,
    pageThesis:: Int
} deriving (Eq, Show)


fBook :: [Book]
fBook = [(Book "AB" "First Book" "Kiev" "p1" 1982),(Book "BB" "Second Book" "Lviv" "p2" 1999),(Book "CB" "Ftird Book" "Moscow" "p3" 2001) ]

fArticle :: [Article]
fArticle = [(Article "AA" "First Article" "magazine1" 2021 8 32),(Article "BA" "Second Article" "magazine2" 1986 24 215)]

fThesis :: [Thesis]
fThesis = [(Thesis "AT" "First Thesis" "conference1" "Kiev" 2018 9)]

applyEach :: [(a -> b)] -> [a] -> [b]
applyEach _ [] = []
applyEach (x:xs) (y:ys) = x y : applyEach xs ys

elem' :: String -> [Book] -> String
elem' _ [] = "False"
elem' x (y : ys) = if x == authorBook y then nameBook y else elem' x ys

elem2' :: String -> [Article] -> String
elem2' _ [] = "False"
elem2' x (y : ys) = if x == authorArticle y then nameArticle y else elem2' x ys

elem3' :: String -> [Thesis] -> String
elem3' _ [] = "False"
elem3' x (y : ys) = if x == authorThesis y then nameThesis y else elem3' x ys

check :: String -> [String]
check a = [elem' a fBook, elem2' a fArticle,elem3' a fThesis]
    