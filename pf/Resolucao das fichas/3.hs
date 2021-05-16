module Ficha3 where
 

data Hora = H Int Int  deriving Show
                              
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

e1= [(H 9 30, H 11 00), (H 11 10, H 12 45), (H 13 30, H 14 45)]
e2= [(H 9 30, H 10 25), (H 11 20, H 10 45), (H 13 30, H 14 45)]



--1
--A 


testaetapa :: Etapa -> Bool
testaetapa (h1,h2) |(testavalida h1 == True) && (testavalida h2 == True) && testaapos h2 h1 == True = True
                   |otherwise = False 

testaapos:: Hora -> Hora ->Bool
testaapos (H h1 m1) (H h2 m2) |h1>h2 = True
                              |h1<h2 = False
                              |h1==h2 && m1 > m2 = True
                              |otherwise= False

testavalida::  Hora  ->Bool
testavalida (H a b )  | a<0 || a > 23 || b <0 || b > 59 = False
                      | otherwise = True 


--B 

testaviagem :: Viagem -> Bool
testaviagem [] = False
testaviagem [e]= testaetapa e 
testaviagem ((h1,h2):(h3,h4) :t) | (testaetapa (h1,h2) == True && testaetapa (h2,h3) == True) = testaviagem ((h3,h4):t)
                                 |otherwise = False

--C

iniciofim :: Viagem -> (Hora,Hora)
iniciofim [(h1,h2)] = (h1,h2)
iniciofim  vi@((h1,h2):(h3,h4) :t) |testaviagem vi == True = (fst (head vi) , snd (last vi))
                                   |otherwise = (H 0 0 ,H 0 0)


--D


calculatempo :: Viagem -> Hora
calculatempo [(h1,h2)] = subtrai h2 h1 
calculatempo ((h1,h2):t) = (convertor(convertor2(adiciona  (subtrai h2 h1 ) (calculatempo t ))))


adiciona :: Hora ->Hora->Hora
adiciona (H a b ) (H c d) = (H (a+c) (b+d))



subtrai :: Hora -> Hora->Hora
subtrai (H a b ) (H c d) = (H (a-c) (b-d))




convertor :: Int -> Hora
convertor x = (H (div x 60) (mod x 60))

convertor2 :: Hora -> Int
convertor2 (H x y) = x*60 + y


--e)

espera :: Viagem ->Hora
espera [(h1,h2)] = (H 0 0)
espera ((h1,h2):(h3,h4):t) =  (convertor(convertor2(adiciona (subtrai h3 h2) (espera ((h3,h4):t)))))


--f)


tempototal :: Viagem ->Hora
tempototal [] = (H 0 0)
tempototal l = (convertor(convertor2 (adiciona (espera l) (calculatempo l ))))  





--2
{-


type Poligonal = [F1.Ponto]

-- a)

comprimento :: Poligonal -> Double
comprimento [p1] = 0
comprimento (p1:p2:ps) = dist p1 p2 + comprimento (p2:ps)

-- b)

linhaFechada :: Poligonal -> Bool
linhaFechada [p1,p2] = False
linhaFechada [p1,p2,p3] = p1 == p3
linhaFechada (p1:p2:p3:ps) = linhaFechada (p1:p3:ps)

-- c)

triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [(Triangulo p1 p2 p3)]
triangula (p1:p2:p3:ps) = (Triangulo p1 p2 p3):triangula (p1:p3:ps)

-- d)

areaPol :: Poligonal -> Double
areaPol p = sum (map (\fig -> area fig) (triangula p))

-- e)

mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p:pol

-- f)

zoom :: Double -> Poligonal -> Poligonal
zoom z [p1@(Cartesiano x y),p2@(Cartesiano a b)] = p1:(Cartesiano (z*a) (z*b)):[]
zoom z (p1@(Cartesiano x y):p2@(Cartesiano a b):pol) = p1:zoom z (p3:pol)
    where p3 = (Cartesiano (z*a) (z*b))

-}









data Contacto = Casa Integer| Trab Integer| Tlm Integer| Email String
            deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

ag1 = [("Alberto",[Email "alberto@gmail.com",Casa 963323562])]

--a)


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n x [] = []
acrescEmail n x (h:t) = (h:t) ++ [(n,[Email x])]

{-
--b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((h,t):cauda) |n== h = Just (filter(==Email )t )
                          |otherwise = verEmails n cauda 
-}


--c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:cs) = case c of Casa x -> x:consTelefs cs
                              Trab x -> x:consTelefs cs
                              Tlm x  -> x:consTelefs cs 
                              otherwise -> consTelefs cs



--d )
casa :: Nome -> Agenda -> Maybe Integer
casa nome [(n,(c:cs))] = if nome == n then case c of Casa x -> Just x
                                                     otherwise -> casa nome [(n,cs)] 
                                      else Nothing
casa nome ((n,c):agenda) = if nome == n then casa nome [(n,c)] else casa nome agenda



--4)

type Dia = Int
type Mes = Int
type Ano = Int


data Data = D Dia Mes Ano
     deriving Show
type TabDN = [(Nome,Data)]


t1 = [("Alberto", D 10 2 2013),("Norberto" , D 12 3 2190),("Albferto", D 10 3 2013),("Norbearto" , D 12 3 2190)]



--a)
procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((a,b):t) | (n == a )= Just  b
                    |otherwise= procura n t 

--b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade n h [] = Nothing
idade u@(D d m ano) n ((a,b):t) | n == a = Just (calculaidade u b )
                              |otherwise =idade u n t 



calculaidade :: Data->Data ->Int
calculaidade (D d m a ) (D d1 m1 a1) |m >m1 || m ==m1 || d >d1 = (a-a1)
                                     |otherwise = (a-a1-1)



--c)
anterior :: Data -> Data -> Bool
anterior (D d m a ) (D d1 m1 a1) | a< a1 = True
                                 | a== a1 && m <m1 = True
                                 |a==a1 && m== m1 && d <d1 = True 
                                 |otherwise = False 


--d) 
ordena :: TabDN -> TabDN
ordena ((h,n):(p,t):cauda) | anterior n t == True = (h,n)