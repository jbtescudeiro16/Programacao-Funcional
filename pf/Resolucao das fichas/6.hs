module Ficha6 where 

data BTree a = Empty 
             | Node a (BTree a) (BTree a) 
             deriving Show

arvore1 = (Node 5 (Node 2 (Node 1 Empty
                                  Empty) 
                          (Node 3 Empty 
                                  Empty)) 
                  (Node 9 (Node 7 (Node 6 Empty 
                                          Empty) 
                                  (Node 8 Empty 
                                          Empty)) 
                          Empty))

--a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node n y x) = 1 + max (altura y) (altura x)

--b)
contaNodos :: BTree a -> Int
contaNodos Empty  = 0
contaNodos (Node n y x) = 1 + (contaNodos y) + (contaNodos x)

--c)
folhas :: BTree a -> Int
folhas Empty = 0 
folhas (Node n Empty Empty) = 1
folhas (Node n x y ) = (folhas x) + (folhas y)

--d)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node h x y ) = (Node h (prune (n-1) x) (prune (n-1) y))
 
--e)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] _ = []
path (True :cauda) (Node h x y ) =  [h]++ (path cauda y )
path (False :cauda) (Node h x y ) = [h]++ (path cauda x )

--f)
mirror :: BTree a -> BTree a 
mirror Empty = Empty 
mirror (Node h x y ) = (Node h (mirror y) (mirror x) )

--g) 
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ =  Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node h l r) (Node n l1 r1) = (Node (f h n) (zipWithBT f l l1) (zipWithBT f r r1))

--h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1,Node b unzipL2 unzipR2,Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r



--2)
--a)
minimo :: Ord a => BTree a -> a
minimo (Node h Empty r) = h
minimo (Node h l r) = minimo l 

--b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node h Empty r ) = Empty
semMinimo (Node h l r) = (Node h (semMinimo l) r) 

--c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty _) = (e,Empty)
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l


remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) | x < e = Node e (remove x l) r
                      | x > e = Node e l (remove x r)
                      | otherwise = aux x (Node e l r)
    where aux n (Node a b c) = case b of Empty -> c
                                         otherwise -> case c of Empty -> b
                                                                otherwise -> Node g b h
          (g,h) = minSmin r

--3)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
     deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))
--a)
inscNum:: Numero -> Turma -> Bool
inscNum n p = (aux n p)


aux:: Numero ->Turma->Bool
aux n Empty = False 
aux n (Node (b,nom,reg,c) l r) | n < b = aux n l
                               | n > b = aux n r
                               |otherwise = True
--b)

nscNome :: Nome -> Turma -> Bool
nscNome n p |(aux1 n p == True ) = True
            |(aux2 n p == True ) = True
            |otherwise = False

aux1 ::Nome -> Turma -> Bool
aux1 n Empty = False
aux1 n (Node (b,nom,reg,c) l r) | (n == nom) = True
                                |otherwise = aux1 n r 

aux2 :: Nome -> Turma -> Bool
aux2 n Empty = False
aux2 n (Node (b,nom,reg,c) l r) | (n == nom ) =  True
                                |otherwise = aux2 n l 


--c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst t@(Node (b,nom,reg,c) l r) = aux4 t



aux4 ::Turma -> [(Numero,Nome)]
aux4 Empty = []
aux4 (Node (b,nom,TE,c) l r) = (aux4 l) ++ [(b,nom)] ++ (aux4 r)
aux4 (Node (b,nom,reg,c) l r)= (aux4 l) ++ (aux4 r)


