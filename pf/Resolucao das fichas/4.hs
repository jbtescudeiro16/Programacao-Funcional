module Ficha4 where

import Data.Char

--1 ) 
-- a) [6,12,18]
-- b) [2,4,6,8,10,12,14,16,18,20]->[6,12,18]
-- c) [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
-- d) [1,1,4,4,9,9,16,16,25,25]




--2 
quadrados=[2^x | x<- [0..10]]
aldrabao = [(x,6-x) | x <- [1..5]]
listagem = [[1..x]| x<-[1..5]]
listagem2 = [replicate x 1| x<-[1..5]]
doisE' =  [product [y | y <- [1..x]] | x <- [1..6]]




--isDigit,isAlpha :: Char -> Bool

--3

--a
digitAlpha :: String -> (String,String)
digitAlpha s = aux1 s ([],[])


aux1::String -> (String,String)->(String,String)
aux1 [] t = t 
aux1 (h:t) (a,b) |isDigit h = aux1 t (h:a,b)
                 |isAlpha h = aux1 t (a,h:b)
                 |otherwise = aux1 t (a,b) 

digitAlpha2 :: String -> (String,String)
digitAlpha2 = foldl (\(num,letra) x -> if isDigit x then (num++[x],letra) else if isAlpha x then  (num,letra ++ [x]) else(num,letra)) ("","")




--4

nzp :: [Int] -> (Int,Int,Int)
nzp a = aux2 a (0,0,0)


aux2 :: [Int] -> (Int,Int,Int) -> (Int,Int,Int)
aux2 [] k = k
aux2 (h:t) (n,j,k)  |(h == 0) = aux2 t (n,j+1,k)
                    |(h<0) = aux2 t (n+1,j,k)
                    |otherwise = aux2 t (n,j,k+1)



 --dúvida
{-
mydivMod :: Integral a => a -> a -> (a, a)
mydivMod x y = aux x y (0,0)
            where
               aux :: Integral a => a -> a -> (a, a) -> (a,a)
               aux x y (d,r) | x - y < 0 = (d, x)
                             | otherwise = aux (x-y) y (d + 1, r)
 -}


 
--6)                        

fromDigits2 :: [Int] ->Int
fromDigits2 h = aux3 h 0 



aux3 ::[Int] ->Int ->Int
aux3 [] _ =  0
aux3 (h:t) l = h*10 ^ length (t) + aux3 t (length t-1)



--7)

--8Ninguem percebe esta merda
