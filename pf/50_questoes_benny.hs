module Test where
--1Feito)
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y = if x > y then [] 
                  else x: enumFromTo1 (x+1) y

enumFromTo2:: Int -> Int -> [Int]
enumFromTo2 x y = [x..y] 
--10 5

--2Feito)
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z | x > z = []
                      |otherwise = x : enumFromThenTo1 y (2*y-x) z


--3Feito)
unir1 :: [a] -> [a] -> [a]
unir1 [] x = x
unir1 (x:xs) y = x: unir1 xs y

--4Feito)
--percorre  a lista compara sempre o 1 elemnto
element :: [a] -> Int -> a
element (x:xs) y | (y == 0) = x
                 | otherwise = element xs (y-1)

--5Feito)
--percorre sempre e envia o 1 elemnto da lista da esquerda para a direita
inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) =  inverte xs ++ [x]

--6Feito)
tirar1 :: Int -> [a] -> [a]
tirar1 x [] = []
tirar1 x (h:t) |x==0 = t
               |otherwise = h:tirar1 (x-1) t 

--7Feito) 
cair :: Int -> [a] -> [a]
cair x []= []
cair x (h:t) | x==1 = t
             |otherwise = cair (x-1) t

--8Feito)
unirpares:: [a] -> [b] -> [(a,b)]
unirpares _ []= []
unirpares []_= []
unirpares (x1:xs) (y1:yt) = (x1,y1) : unirpares xs yt

--9Feito)
elem1 :: Eq a => a -> [a] -> Bool
elem1 x [] = False
elem1 x (h:t)|x == h =True
             |otherwise = (elem1 x t) 
--10Feito)
replicar::Int -> a -> [a]
replicar 0 _= []
replicar x y = y: replicar (x-1) y 

--11Feito)

intercalar :: a ->[a] -> [a]
intercalar _ [] = []
intercalar x (h:t) = h:x :intercalar x t

--12)
agrupa :: Eq a => [a] -> [[a]]
agrupa [] = [[]]
agrupa l = [l] ++ agrupa (drop 1 l) 

--13Feito mas experimentarcom [[]])
concatena2:: [[a]]->[a]
concatena2 []= []
concatena2 (h:hs) = h ++ concatena2 hs

-- 14 Feito-perceber como funciona)
inicial :: [a] -> [[a]]
inicial [] = [[]]
inicial x = inicial (init x) ++ [x]

--15
caudas :: [a] -> [[a]]
caudas [] = [[]]
caudas l = [l] ++ caudas (take (length (l)-1) l) 

--16
testarprefixo :: Eq a => [a] -> [a] ->Bool
testarprefixo [] _ = True
testarprefixo _ [] = False
testarprefixo (h1:hs) (t1:ts) = h1==t1 && testarprefixo hs ts

--17 
testarSufixo :: Eq a => [a] ->[a] -> Bool
testarSufixo [] _ = True
testarSufixo _ [] = False
testarSufixo l (y1:ys) = l == (y1:ys) || testarSufixo l ys 

--18
testarConseq :: Eq a => [a] -> [a] -> Bool
testarConseq [] _ = True
testarConseq _ [] = False
testarConseq  (h1:hs) (t1:ts) = h1==t1 && testarConseq hs ts || testarConseq (h1:hs) ts

--19
--elemLugar :: Eq a => a -> [a] -> [Int]
--elemLugar _ [] = []
--elemLugar x (y:ys) |if x==y | 


--20 

--apresentaelementos::Eq a => [a] -> [a]
--apresentaelementos [x] = [x]
--apresentaelementos [] = []
--apresentaelementos (x:h:xs) =  if (x == h)
-- then x : apresentaelementos (x:xs)
--else x:apresentaelementos (h:xs)


--21
apagaelementos :: Eq a => a -> [a] ->[a]
apagaelementos x [] = []
apagaelementos x (h:xs) |(x==h) = xs
                        |otherwise = h:apagaelementos x xs

--22
remove2 :: Eq a => [a]-> [a]-> [a]
remove2 l [] = l
remove2 [] _ = [] 
remove2 l (h1:hs) = remove2 (apagaelementos h1 l) hs

--23
juntar2 :: Eq a => [a] -> [a] -> [a]
juntar2 l [] = l
juntar2 l (h:t)
    | (elem h l == True) = juntar2 l t
    | otherwise = juntar2 (l ++ [h]) t

--24
removeiguais:: Eq a => [a] ->[a] -> [a]
removeiguais [] _ = []
removeiguais (x:xs) l |(elem x l == True) = x: removeiguais l xs
                      | otherwise = removeiguais xs l   

--25
insereeord ::  Ord a => a -> [a]-> [a] 
insereeord x [] = [x]
insereeord x (h:t) |(x>h) = h: insereeord x t
                   |otherwise = x:h:t



--26
insereString :: [String] -> String
insereString [] = ""
insereString (h:t)=h ++ (if t == [] then "" else " ") ++insereString t            


--27
uneStringnn :: [String] -> String
uneStringnn [] = ""
uneStringnn (x:xs) = x ++ "\n" ++ uneStringnn xs

--28
--pmaior 


--29
temrepetidos::Eq a => [a] -> Bool
temrepetidos []= False
temrepetidos (x:xs) = (elem x xs) || temrepetidos xs

--30 
algarismos :: [Char]->[Char]
algarismos [] = []
algarismos (x:xs) | elem x ['0'..'9'] = x:algarismos xs
                  |otherwise = algarismos xs 
                    
--31

posImpares ::  [a] -> [a]
posImpares [] = []
posImpares [_] =[]
posImpares (x:t:xs) = t:posImpares xs

--32
posPares ::  [a] -> [a]
posPares [] = []
posPares [_] = []
posPares (x:t:xs) = x : posPares xs

--33
isSorted ::  Ord a => [a] -> Bool
isSorted [] = True
isSorted [l] =True
isSorted (x:h:xs) = x<=h && isSorted (h:xs)

--34
ins ::  Ord a => [a] -> [a]
ins [] = []
ins (x:xs) = insereeord x (ins xs)


--35 
--menor ::  String -> String -> Bool
--
--menor _  "" = False
--menor "" _ = True

--36
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((a,b) :t)
   | x==a =True
   |otherwise = elemMSet x t

--37
lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):t) = b + lengthMSet t

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x:converteMSet xs
converteMSet ((x,n):xs) = x:converteMSet ((x,n-1):xs)

--39 
insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) |(x==a) = (a,b+1):t
                       |otherwise = (a,b):insereMSet x t

--40
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((x,y):t) 
  | n == x = t
  |otherwise = (x,y):removeMSet n t

  --41
constroiMSet ::  Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

--42
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right x):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left x):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls


--43 
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a):ms) = a:catMaybes' ms
catMaybes' (Nothing:ms) = catMaybes' ms


--44
data Movimento = Norte | Sul | Este | Oeste  deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (m:ms) = posicao (case m of Norte -> (x, y + 1)
                                           Sul -> (x, y - 1)
                                           Este -> (x + 1, y)
                                           Oeste -> (x - 1, y)) ms

--45


caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xi < xf = Este:caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste:caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte:caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul:caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

--46 pq n pode ser assim?

--vertical ::[Movimento] -> Bool
--vertical [] = True 
--vertical (x:xs)  
  -- | (x == Norte) = vertical xs
  -- | (x == Sul) = vertical xs
  -- | otherwise = False 


--47
data Posicao = Pos Int Int deriving Show


maisCentral' :: [Posicao] -> Posicao
maisCentral' [(Pos x y)] = (Pos x y)
maisCentral' ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral' ((Pos x y):ps) else maisCentral' ((Pos a b):ps)


--48
vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' _ [] = []
vizinhos' (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                       then (Pos xv yv):vizinhos' (Pos x y) ps 
                                       else vizinhos' (Pos x y) ps



--49                                       
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada ((Pos x2 y2):ps)


