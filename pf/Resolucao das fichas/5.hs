module Ficha5 where 


--1
--a

any1 :: (a -> Bool) -> [a] -> Bool
any1 h [] = True
any1 h (m:s) | h m == True  = any1 h s 
             | h m == False = False 

--b)
zipWith1 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith1 n _ [] = []
zipWith1 n [] _ = []
zipWith1 n (h:s) (t:u) = (n h t ): zipWith1 n s u 


--c) 
takeWhile1 :: (a->Bool) -> [a] -> [a]
takeWhile1 n [] = []
takeWhile1 n (h:t) | (n h == True) = [h] ++ (takeWhile1 n t) 
                   |otherwise = []

--d)
dropWhile1 :: (a->Bool) -> [a] -> [a]
dropWhile1 h [] = []
dropWhile1 h (c:y) | h c == True = dropWhile1 h y
                   | otherwise = y

--e)

span1 :: (a-> Bool) -> [a] -> ([a],[a])
span1 h (a:b) |(h a == True) = (a:s1,s2)
              | otherwise = ([],(a:b))
     where (s1 ,s2) = span1 h b 
 
--f)
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f n [] = []
deleteBy f n (h:t) | f n h == True = t 
                   |otherwise = [h] ++ deleteBy f n t 

--sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]


--g)
{-
sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insere (h) (sortOn f t)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a then a:insere x b else x:a:b
-}

--2 




type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n ps  = filter (\ x -> snd x == n) ps

--b ) 
conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p) 

--c)
grau :: Polinomio -> Int
grau p = foldl (\grau x -> if grau > snd x then grau else snd x ) 0 p 

--d)
deriv :: Polinomio -> Polinomio
deriv p = map (\ (b,e) -> ((fromIntegral e) * b ,(e-1))) p 

--e)
calcula :: Float -> Polinomio -> Float
calcula n p = foldl (\soma (b,e) -> (b *(n^e)) +soma ) 0 p

--f)
simp :: Polinomio -> Polinomio
simp p = filter (\ n -> (fst n )/= 0) p

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult (b,a) p  = map (\ (e,c) -> (b*e,a+c)) p 




--3)

type Mat a = [[a]]

--Mat 1 = [[1,2,3],[3,4,5],[5,6,7]]



dimOK :: Mat a -> Bool
dimOK [] = True
dimOK [x] = True
dimOK (h:c:t) | (length h == length c) = dimOK (c:t)  
              | otherwise = False




