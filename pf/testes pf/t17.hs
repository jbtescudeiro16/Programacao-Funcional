module T1718 where


insert :: Ord a => a -> [a] ->[a] 
insert n [] = [n]
insert n (h:t) | n >= h = h:( insert n t )
               | otherwise = (n:h:t) 

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of (Just a) -> a:catMaybes xs
                             Nothing -> catMaybes xs


--2)
data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)



instance Show a => Show (Exp a) where
  show (Const a) =  (show a) 
  show (Var c) = c 
  show (Mais a b) = "(" ++ (show a) ++ "+" ++ (show b) ++ ")"
  show (Mult a b) = "(" ++ (show a) ++ "x" ++ (show b) ++ ")"



sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f [x] = [x]
sortOn f (a:b:c) | f a < f b = a:sortOn f(b:c) 
                 |otherwise = b:sortOn f (a:c )



sortOn2 :: Ord b => (a -> b) -> [a] -> [a]
sortOn2 f [] = []
sortOn2 f [x] = [x]
sortOn2 f (a:c) = insere a (sortOn2  f c )
     where insere a [] = [a]
           insere a (b:c) | (f a < f b) = a:insere b c 
                          |otherwise = b:insere a c             


amplitude2 ::  [Int] -> Int
amplitude2 [] = 0
amplitude2 l = mx - mn
    where (mx,mn) = foldl (\(a,b) n -> (if n > a then n else a,if n < b then n else b)) (head l,head l) l 



amplitude :: [Int] -> Int
amplitude [] = 0
amplitude h = (a-b)
 where a = maximum h
       b = minimum h



data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]



conta :: Imagem -> Int
conta (Quadrado a) = 1
conta (Mover a b) = conta b 
conta (Juntar (h:ht)) = sum (map conta (h:ht)) 