module Teste1920 where 


--1)

intersect :: Eq a => [a] -> [a] -> [a] 
intersect [] _ = []
intersect _ [] = []
intersect (a:b) t |elem a t == True = a:intersect b t 
                  |otherwise = intersect b t 
--2)
tails1 :: [a] -> [[a]]
tails1 [] = [[]]
tails1 l = [l] ++ tails1 (tail l )




type ConjInt = [Intervalo]
type Intervalo = (Int,Int)


--2)

--a
elems :: ConjInt -> [Int]
elems [] = []
elems ((a,b):hs) = (enumFromTo a b) ++  elems hs 


--b
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (h : t) = (h, d) : geraconj (dropWhile (<= d) t)
    where d = foldl (\acc x -> if x == succ acc then x else acc) h t


data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
   deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]


agenda1 = [("Sofia", [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz"]),("Sofia", [Tlm 69420]),("Rita", [Trab 58008])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email  [] = [(nome,[(Email email)])]
acrescEmail nome email a  = a ++ [(nome,[(Email email)])]


--b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails a ag@(((n),(h:t)):cauda) | ((aux a ag)  == True )=  Just (procuramail a ag) 
	                               |otherwise = Nothing 

aux :: Nome -> Agenda ->Bool
aux n [] = False
aux a ag@((n,(h:t)):cauda) | a== n = True
                           |otherwise = (aux a cauda)


procuramail:: Nome ->Agenda -> [String]
procuramail a (((n),(h:t)):cauda) | a == n = pc (h:t)
                                  |otherwise= procuramail a cauda


pc :: [Contacto] -> [String]
pc [] = []
pc ((Email a) :t) = [a] 
pc ((Casa a) :t) = pc t
pc ((Trab a) :t) = pc t
pc ((Tlm a):t) = pc t



ct = [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz"]

--c)
consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta  (h:t) = case h of Email x -> (a,b ++ [x])
                            Casa x  -> (a ++ [x],b)
                            Trab x  -> (a ++ [x],b)
                            Tlm  x  -> (a ++ [x],b)
             where (a,b) = consulta t


--d)


consultaIO :: Agenda -> IO ()
consultaIO a = do 
	   putStrLn "Insira o nome que deseja ver os Contactos associados"
	   nome <- getLine 
	   let busca1 =  (busca nome a)
	   putStr (concat [show x ++ "\n" | x <- busca1])
	   

busca a [] = []
busca a (((n),ct) :cauda) | a == n = ct 
                          | otherwise =  busca a cauda 

{-
consultaIO :: Agenda -> IO ()
consultaIO agenda = do
    nome <- getLine
    let contactos = aux nome agenda
    putStr (concat [show x ++ "\n" | x <- contactos])

    where aux _ [] = []
          aux nome ((name,contactos):t) = if name == nome then contactos else aux nome t
 -}



data RTree a = R a [RTree a] deriving (Show, Eq)

tree1 = R 1 [R 2 [],
             R 3 [R 4 [R 5 [],
                       R 6 []
                      ]
                 ],
             R 7 []
            ]



paths :: RTree a -> [[a]] 
paths (R a []) = [[a]]
paths (R n l) = map (\x -> n:x) c
      where c = concat (map paths l)