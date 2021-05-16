module Questoes where 

--1
enumfromto1:: Int->Int -> [Int]
enumfromto1 x y =if x >y then []
		else x: enumfromto1 (x+1) y
--2
enumfromthento1 ::Int->Int ->Int -> [Int]
enumfromthento1 x y z |  x>z = []
				|otherwise= x:enumfromthento1 y (2*y-x) z 
--3
concatena ::[Int]->[Int] ->[Int]
concatena [] x= x
concatena x [] = x
concatena (x:xs) y = x :concatena xs y
--4
posicoes :: [a] -> Int -> a
posicoes (x:xs) n |(n== 0) = x
                  |otherwise =  posicoes xs (n-1)
--5
reverse1 ::  [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) =  reverse1 xs ++ [x]
--6
takes ::  Int -> [a] -> [a]
takes 0 l = []
takes _ [] = []
takes a (h:t) = h: takes (a-1) t 
--7
drop1 ::  Int -> [a] -> [a]
drop1  _ [] = []
drop1 0 l = l
drop1 n (x:xs) =if (n==1) then xs
			   else  drop1 (n-1) xs
--8
zip1 ::  [a] -> [b] -> [(a,b)]
zip1 _ [] = []
zip1 [] _ = []
zip1 (x:xs) (h:t) = [(x,h)] ++ zip1 xs t
--9
elem1 ::  Eq a => a -> [a] ->Bool
elem1 _ [] = False
elem1 n (x:xs) | n== x =True
               |otherwise = elem1 n xs
 --10
replicate1 ::  Int -> a ->[a]
replicate1 0 _ = []
replicate1 n q = q : replicate1 (n-1) q
--11
intercalar1 ::  a -> [a] ->[a]
intercalar1 _ [] = []
intercalar1 n (x:xs) = [x]++[n]++intercalar1 n xs
--12
agrupa :: Eq a => [a] -> [[a]]
agrupa [] = []
agrupa (h:t) = (h:takeWhile (== h) t) : agrupa (dropWhile (== h) t)

--13
concat1 ::  [[a]] -> [a]
concat1 [[]] = []
concat1 (h:t)= h++concat1 t
--14
inicial :: [a] -> [[a]]
inicial [] = [[]]
inicial x = inicial (init x) ++ [x]
--15
tails ::  [a] -> [[a]]
tails [] = [[]]
tails x = [x] ++ tails (tail x)
--16
testarpref::  Eq a => [a]-> [a] -> Bool
testarpref _ [] = False
testarpref [] _ =True
testarpref (x:xs) (h:t) = x==h && testarpref xs t
--17
testarsufix :: Eq a => [a]-> [a] -> Bool
testarsufix [] _ =True
testarsufix _ [] = False
testarsufix l (h:t) = l==(h:t) || testarsufix l t
--18
vemporord::Eq a => [a]-> [a] -> Bool
vemporord [] _= True
vemporord _ [] =False
vemporord (xh:xt) (yh:yt)
                | xh == yh = vemporord xt yt 
                | otherwise = vemporord (xh:xt) yt
--19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t) 
    | x == h = 0 : map (+1) (elemIndices' x t)
    | otherwise = map (+1) (elemIndices' x t)

--20
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) = h : filter (/= h) (nub1 t)

--21
delete1 ::  Eq a => a -> [a]-> [a]
delete1 _ [] = []
delete1 n (h:t) |(n==h) = t
                |otherwise =h:delete1 n t
--22
delete2::  Eq a => [a] -> [a]-> [a]
delete2 [] x = x
delete2 _ [] = []
delete2  l (x:xs) = delete2 (delete1 x l) xs
--23
union1 ::  Eq a => [a] -> [a]-> [a]
union1 x [] = x 
union1 l (x:xs) | elem x l == True = union1 l xs
                | otherwise = union1 (l++[x]) xs
--24
intersect1 ::  Eq a => [a] ->[a] -> [a]
intersect1 [] _ = []
intersect1 (x:s) l 
  | elem x l = x: intersect1 s l
  |otherwise = intersect1 s l
--25
insert1 ::  Ord a => a -> [a]-> [a]
insert1 x [] = [x]
insert1 n (x:xs) |n <= x = (n:x:xs)
	|otherwise = x:insert1 n xs
--26
unwords1 ::  [String] -> String
unwords1 [] = ""
unwords1 [x]= x
unwords1 (h:t) = h ++ " " ++ (unwords1 t)
--27
unlines1 ::  [String] -> String
unlines1 [] = ""
unlines1 (x:xs) = x ++ "\n" ++ unlines1 xs

--28

--29
temRepetidos1 ::  Eq a => [a] -> Bool
temRepetidos1 [] = False 
temRepetidos1 (x:xs) | (elem x xs ==True) = True
					 |otherwise = temRepetidos1 xs
--30
algarismos1 ::  [Char] -> [Char]
algarismos1 [] = []
algarismos1 (x:xs) |elem x ['0'..'9'] = [x] ++ algarismos1 xs
				   |otherwise = algarismos1 xs					
--31
posImpares1 ::  [a] -> [a]
posImpares1 [] = []
posImpares1 [_]=[]
posImpares1 (x:h:xs) = [h]++posImpares1 xs
--32
posPares1 ::  [a] -> [a]
posPares1 [] = []
posPares1 [x]= [x]
posPares1 (x:h:xs) = [x]++ posPares1 xs
--33
isSorted1 ::  Ord a => [a] -> Bool
isSorted1 [] = True 
isSorted1 [x] = True
isSorted1 (x:h:xs)|x<= h = isSorted1 (h:xs)
                  |otherwise=False
--34
ins ::  Ord a => [a] -> [a]
ins [] = []
ins (x:xs) = insert1 x (ins xs)
--35
menor1 ::  String -> String -> Bool
menor1 [] l = True
menor1 (x:xs) (y:ys)
      | x > y = False
      | x < y = True
      | x == y = menor1 xs ys
--36
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet n ((x,y):t) | n == x = True
                     |otherwise = elemMSet n t
--37
lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0 
lengthMSet [(s,n)] = n
lengthMSet ((h,a):t) = a + lengthMSet t
--38
converteMSet ::  [(a,Int)] -> [a]
converteMSet [] = []
converteMSet [(a,x)] = replicate x a 
converteMSet ((p,t):s) = replicate t p ++ converteMSet s
--39
insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet n ((a,b):t) | (n == a) = ((a,b+1):t)
                       | otherwise = [(a,b)]++ insereMSet n t 
--40
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet n ((h,t):r) | (n == h) && (t ==1) = r
                       | (n == h) && (t/= 1 )= ((h,t-1):r)
                       |otherwise = [(h,t)]++removeMSet n r 
--41
constroiMSet1 ::  Ord a => [a] -> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 (x:xs) = (x , (1 + length(filter (==x) xs))) : constroiMSet1 (filter(/=x) xs)
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
catMaybes1 ::  [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 ((Just x) :xs) = x:catMaybes1 xs
catMaybes1 ((Nothing ):xs) = catMaybes1 xs
--44
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao1 ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao1 x [] = x
posicao1 (x,y) (m:ms)  = (case m of 
	                       Norte -> posicao1 (x,y+1) ms 
	                       Sul -> posicao1 (x,y-1) ms
	                       Este -> posicao1 (x+1,y) ms
	                       Oeste -> posicao1 (x-1,y) ms ) 
--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xi < xf = Este:caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste:caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte:caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul:caminho (xi, yi - 1) (xf, yf)
    | otherwise = []
--46
vertical ::  [Movimento] -> Bool
vertical [] = False
vertical [Norte] = True 
vertical [Sul] =True 
vertical (Este:t) =False
vertical (Oeste:t) =False
vertical (Norte:t) = vertical t
vertical (Sul :t)= vertical t 

data Posicao = Pos Int Int deriving Show
--47
maisCentral ::  [Posicao] -> Posicao
maisCentral [(Pos x y)] = (Pos x y)
maisCentral ((Pos a b):(Pos c d ):t) 
     | (a^2 + b^2) > (c^2 + d^2)  = maisCentral ((Pos c d ):t)
     |otherwise = maisCentral ((Pos a b):t)


--48 perceber melhr
--vizinhos' :: Posicao -> [Posicao] -> [Posicao]
--vizinhos' _ [] = []
--vizinhos' (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
 --                                      then (Pos xv yv):vizinhos' (Pos x y) ps 
 --                                      else vizinhos' (Pos x y) ps


 --49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada ((Pos x2 y2):ps)
--50

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK x = sum (map auxiliar x) <2

auxiliar :: Semaforo ->Int
auxiliar Verde = 1
auxiliar Amarelo = 1
auxiliar Vermelho = 0
