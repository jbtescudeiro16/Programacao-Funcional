module Teste1819 where 

import Data.Char

elemIndices ::  Eq a => a -> [a] -> [Int]
elemIndices a [] = []
elemIndices a h = aux 0 a h

aux :: Eq a => Int -> a -> [a] -> [Int]
aux n a [] = []
aux n a (h:t) | (a==h) = [n] ++ (aux (n+1) a t )
              |otherwise = (aux (n+1) a t )


elemIndices2 ::  Eq a => a -> [a] -> [Int]
elemIndices2 a [] = []
elemIndices2 a (h:t) |a==h =  0: map (+1) (elemIndices2 a t)  
                     |otherwise = map (+1) (elemIndices2 a t) 



--b)

isSubsequenceOf ::  Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (a:b) (c:d) = (a == c) && isSubsequenceOf b d || isSubsequenceOf (a:b) d 


--2)

data BTree a = Empty | Node a (BTree a) (BTree a)
{-
lookupAP ::  Ord a => a -> BTree (a,b) -> Maybe b
lookupAP a Empty = Nothing
lookupAP a (Node c Empty Empty) |a==c = (Just a) 
                                |otherwise = Nothing
lookupAP a ( Node c  d e) |a == c = (Just a) 
                          |a < c  =  lookupAP a d 
                          |a > c  = lookupAP a e
                          |otherwise = Nothing
 -}


arvore1 = (Node (5,4) (Node (7,4) (Node (2,3) Empty
                                  Empty) 
                          (Node (2,4) Empty 
                                  Empty)) 
                  (Node (6,4) (Node (1,4) (Node (2,6) Empty 
                                          Empty) 
                                  (Node (2,8) Empty 
                                          Empty)) 
                          Empty))





lookupAP ::  Ord a => a -> BTree (a,b) -> Maybe b
lookupAP a Empty = Nothing
lookupAP a (Node (b,c) Empty Empty)= if a == b then Just c else Nothing
lookupAP a (Node (b,c) g h) | a == b = Just c
                            | a < b = lookupAP a g
                            |otherwise = lookupAP a h  






--b)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT h Empty _= Empty 
zipWithBT h (Node a b c) (Node d e f) = (Node (h a d)  (zipWithBT h b e) (zipWithBT h c f)) 

--3

digitAlpha3 :: String -> (String,String)
digitAlpha3 [] = ([],[])
digitAlpha3 (t:ts) = foldl (\(a,b) t  -> if (isDigit t == True ) then  ([t]++a,b) else if (isAlpha t == True ) then (a,[t]++b) else (a,b)) ([],[]) ts


digitAlpha2:: String-> (String,String)
digitAlpha2 [] = ([],[])
digitAlpha2 (h:t)
  |(isDigit h == True) = (c,[h]++b) 
  |(isAlpha h == True) = ([h]++c,b)
  |otherwise = (c,b)
 where (c,b) = digitAlpha2 t 

--4)
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

--a)
firstSeq ::  Seq a -> a
firstSeq a = case a of Nil -> error "Nothing"
                       Cons a _ -> a
                       App (Nil) c ->  firstSeq c
                       App a c -> firstSeq a  



--b)
dropSeq :: Int -> Seq a -> Seq a
dropSeq n Nil = Nil
dropSeq n (Cons a x) = (dropSeq (n-1) x)
dropSeq n (App a b) | (n<(contanodos a)) = (dropSeq (n-1) a)
                    | otherwise = (dropSeq (n-(contanodos a)) b)  


contanodos :: Seq a -> Int
contanodos Nil = 0
contanodos (Cons a x) = 1 + (contanodos x)
contanodos (App a b) = (contanodos a) + (contanodos b)



--c)
instance Show a => Show (Seq a) where
    show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2
 



type Mat a = [[a]]


magic ::  Mat Int -> Bool