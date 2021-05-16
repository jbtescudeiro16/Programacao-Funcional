module Ficha2 where
import Data.Char

-- 2)
--a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h) : dobros t 

dobros2 :: [Float] -> [Float]
dobros2 [] = []
dobros2 l =  [2*x | x<-l]

--b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0 
numOcorre n (h:t) |n == h = 1+ numOcorre n t 
                  | otherwise = numOcorre n t
--c)
positivos :: [Int] -> Bool 
positivos [] = True
positivos (x:xs) |x> 0 && positivos xs = True
                 | otherwise = False

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) | x>0 = x: soPos xs
             |otherwise = soPos xs
--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) | x<= 0 = x+ somaNeg xs
               |otherwise = somaNeg xs

--f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) |length (x:xs) >3 = tresUlt (xs)
               |length (x:xs) == 3 = (x:xs)
               |otherwise = (x:xs)

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos((a,b):t) = [b]++ segundos t 


--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros n ((a,b):t) | n == a = True
                     |otherwise = nosPrimeiros n t

--i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):ts) = (x + sumX, y + sumY, z + sumZ)
        where (sumX, sumY, sumZ) = sumTriplos ts



--COPIADO (POUCO IMPORTANTE)

--3



isLowerN:: Char -> Bool
isLowerN x = ord x >= 97 && ord x <= 122

isDigitN:: Char -> Bool
isDigitN x = ord x >= 48 && ord x <= 57

digitToIntN:: Char -> Int
digitToIntN x = (ord x) - 48



soDigits:: [Char] -> [Char]
soDigits [] = []
soDigits (h:t) = if isDigitN h
                 then h : soDigits t
                 else soDigits t

minusculas:: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if isLowerN h
                   then 1 + minusculas t
                   else minusculas t

nums:: String -> [Int]
nums "" = []
nums (h:t) = if isDigitN h
             then digitToIntN h : nums t
             else nums t

--4)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

poli1 = [(2,3), (3,4), (5,3), (4,5)] 

--a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((a,b):t) | (n == b ) = 1+ conta n t 
                  |otherwise = conta n t
--b)
grau :: Polinomio -> Int
grau [(a,b)] = b
grau ((a,b):t)  | b > grau t = b
                  |otherwise = grau t
--c)
 {-

 -- para (Int,Int )
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) |b>0 = (a*b ,b-1) : deriv t  
                |otherwise = deriv t 


-}
--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((a,b):xs) | n == b = (a,b) : selgrau n xs
                     |otherwise = selgrau n xs

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((b,e):ps) = if e > 0 then (b*fromIntegral e,e-1):deriv ps else deriv ps -- from integral "transforma" FLOATS EM INTEGER 

--e)
calcula1 :: Float -> Polinomio -> Float
calcula1 _ [] = 0
calcula1 n ((a,b):t) = a*(n^b) + calcula1 n t  

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) | a == 0 = simp t 
               | otherwise =  (a,b) : simp t 

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((y,c):t) = (a*y,b+c) : mult (a,b) t  

--h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(b,e)] = [(b,e)]
normaliza ((b,e):(b2,e2):ps) = if e == e2 then normaliza ((b+b2,e):ps) 
                                           else if conta e ps == 0 then (b,e):normaliza ((b2,e2):ps)
                                                                   else normaliza ((b,e):ps ++ [(b2,e2)])
--i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)


--j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (p:p1) p2 = soma (mult p p2) (produto p1 p2) 

--k)
{-
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b),(c,d) :t) | b<d = (a,b) : ordena (c,d) t
                        | b== d =  ordena (a+c,d) t
                        |otherwise = (c,d) : ordena (a,b) t

-}



--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)