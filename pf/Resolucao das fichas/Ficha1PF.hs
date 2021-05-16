module Ficha1 where
import Data.Char

--1
--a)

perimetro::Double-> Double	
perimetro x = 2 * pi * x 

--b)
dist:: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1 - y2)^2)


--c)
primUlt :: [a] -> (a, a)
primUlt l = (head l, last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo n m | (mod n m == 0) =True
             | otherwise = False

--e)
truncaimpar:: [a] -> [a]
truncaimpar l | odd (length l) == True = drop 1 l  
              |otherwise = l
--f)
max2 :: Int ->Int ->Int
max2  x y | x >=  y = x 
          | otherwise = y
--g)
max3 :: Int ->Int ->Int ->Int
max3 x y z = max2 x (max2 y z )

--2
--a) 
nRaizes :: Double ->Double->Double ->Int
nRaizes a b c |delta < 0 = 0 
              |delta == 0 = 1
              |otherwise = 2 
        where delta = (b^2 - 4*a*c)

--b)
raizes:: Double -> Double -> Double -> [Double]
raizes a b c = if nRaizes a b c /= 0
                    then if nRaizes a b c == 2
                         then [(((-b) + sqrt((b^2) - 4*a*c))/(2*a)), (((-b) - sqrt((b^2) - (4*a*c)))/(2*a))]
                         else [((-b)/(2*a))]
               else []

type Hora = (Int,Int)

--3
--a)

testavalida::  (Int,Int) ->Bool
testavalida (a,b) | a<0 || a > 23 || b <0 || b > 59 = False
                  | otherwise = True 

--b)
testaapos:: Hora -> Hora ->Bool
testaapos (h1,m1) (h2,m2) |h1>h2 = True
                          |h1<h2 = False
                          |h1==h2 && m1 > m2 = True
                          |otherwise= False

--c) 
convertmin :: Hora -> Int
convertmin (a,b) = b+ (a*60)

--d)
convertHora:: Int -> Hora
convertHora x = verHora((div x 60), x - (div x 60) *60)


verHora:: Hora -> Hora
verHora (x,y) = if x >= 24
                then ((div x 24 - 1), y)
                else (x,y)

--e) 
calculadif :: Hora ->Hora ->Int
calculadif (h1,h2) (h3,h4) = abs ( convertmin(h1,h2) - convertmin (h3,h4))

--f) 
adicionamin :: Hora ->Int ->Hora
adicionamin (h,m) a = convertHora(convertmin(h,m)+a)  




data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--5  
--a) 
next ::Semaforo ->Semaforo
next Verde = Amarelo
next Vermelho = Verde
next Amarelo = Vermelho

--b) 

stop :: Semaforo  ->Bool 
stop Vermelho = True
stop Verde = False
stop Amarelo = False

--c)
testaSafe :: Semaforo ->Semaforo ->Bool
testaSafe s1 s2 |s1 == Vermelho || s2 == Vermelho = True
                |otherwise = False
--6)

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--a) 
posx:: Ponto -> Double
posx (Cartesiano x _) = x 
posx (Polar x a) = x * cos (a)

--b)

posy:: Ponto ->Double
posy (Cartesiano _ y) = y
posy (Polar x a ) = x * sin a 

--c) 

raio:: Ponto -> Double
raio (Cartesiano x y) = sqrt((x^2) + (y^2))
raio (Polar x _ ) = x

--d)

angulo:: Ponto -> Double
angulo (Cartesiano x y) = atan(y/x)
angulo (Polar _ a) = a

--e)

distPonto:: Ponto -> Ponto -> Double
distPonto a b = sqrt(((posx a - posx b)^2) + ((posy a - posy b)^2))

--7)
data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show, Eq)
--a)

poligono :: Figura -> Bool
poligono (Circulo c r) = r > 0 -- Verifica que o raio é positivo
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2 -- Verifica que os pontos não têm o mesmo valor de x ou y
poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1) / (posx p2 - posx p1) /= (posy p3 - posy p2) / (posx p3 - posx p2) -- Verifica que os pontos não pertencem todos à mesma reta

--b)

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices retang@(Retangulo p1 p2) = if poligono retang then [(p1), (Cartesiano (posx p1) (posy p2)), (Cartesiano (posx p2) (posy p1)), (p2)] else []
vertices triang@(Triangulo p1 p2 p3) = if poligono triang then [p1, p2, p3] else []

--c)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1) 

--d)

perimetro1 :: Figura -> Double
perimetro1 (Circulo _ r) = 2 * pi * r
perimetro1 (Retangulo p1 p2) = 2 * abs (posx p2 - posx p1) + 2 * abs (posy p2 - posy p1)
perimetro1 (Triangulo p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p1 p3
