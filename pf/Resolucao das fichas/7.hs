module Ficha7 where 

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt



--a

calcula :: ExpInt -> Int
calcula (Const num) = num
calcula (Simetrico a)  = (- calcula  a )
calcula (Mais a b) = (calcula  a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)


--b 

infixa :: ExpInt -> String
infixa (Const num) =  show  num 
infixa (Simetrico a)  = "(-" ++ infixa a  ++ ")"
infixa (Mais a b) =  '(' : (infixa a)++ " " ++ "+" ++ " " ++ (infixa b) ++ ")"
infixa (Menos a b) =  '(' : (infixa a)++  " " ++ "-" ++ " " ++ (infixa b) ++ ")"
infixa (Mult a b) = '(' : (infixa a)++  " " ++ "*" ++ " " ++ (infixa b) ++ ")"


--c

posfixa :: ExpInt -> String
posfixa (Const num) =  show  num 
posfixa (Simetrico a)  = (posfixa a ) ++ "-"
posfixa (Mais a b) = (posfixa a) ++ (posfixa b) ++ "+"
posfixa (Menos a b) =(posfixa a) ++ (posfixa b) ++ "-"
posfixa (Mult a b) = (posfixa a) ++ (posfixa b) ++ "*"



--2





data RTree a = R a [RTree a]


rtree1 = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []]]
--a)

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a b) = a + sum (map (soma) b) 

--b)
altura1 :: RTree a -> Int
altura1 (R e []) = 1
altura1 (R e es) = 1 + maximum (map altura1 es)


--C)
prune :: Int -> RTree a -> RTree a
prune 0 (R e b) = (R e [])
prune n (R e b) = (R e (map (prune (n-1))  b) )



mirror :: RTree a -> RTree a
mirror (R e []) = (R e [])
mirror (R a b ) =   (R a (map mirror (reverse b )))


postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R a b)  =  (concatMap postorder b) ++ [a] 


data BTree a = Empty | Node a (BTree a) (BTree a)


data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

ltree1 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))

--a)

ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a b) = ltSum a + ltSum b



--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

--c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork a b) =  1 + max (ltHeight a) (ltHeight b)




--4

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

ftree1 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))

