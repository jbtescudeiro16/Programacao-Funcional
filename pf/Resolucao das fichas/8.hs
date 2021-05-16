module Ficha8 where  


import Data.List
import Data.Char


data Frac = F Integer Integer 

--A

normaliza :: Frac -> Frac
normaliza (F a b ) |a>0 && b > 0 = F ( div a (mdc a b)) (div b (mdc a b))
                   |a>0 && b < 0 = F (div a (mdc a (abs b)))  (-(div (abs b) (mdc a (abs b))))
                   |a<0 && b > 0 = F (div (abs a) (mdc (abs a) b))  (-(div b (mdc (abs a) b)))
                   |otherwise =  F (div (abs a) (mdc (abs a) (abs b)))  (div (abs b) (mdc (abs a)  (abs b)))


mdc :: Integer ->Integer->Integer 
mdc a b | (a == b) = a 
        | (a > b) = mdc (a - b) b 
        | otherwise = mdc b a 

--B

instance  Eq Frac where
 (F a b ) == (F c d) = a*d == b*c


 --c

instance Ord Frac where
 (F a b) <= (F d c ) = b*d >= a*c 

 --d)

instance Show Frac where
  show (F a b) = show a ++ "/" ++  show b 

 --e)
instance Num Frac where
 (F a b) + (F c d)  | b == d = (normaliza (F (a+c) d))  
                    | otherwise =  normaliza (F ((a*d) + (b*c)) (b*d))
 (F a b) - (F c d)  | b == d = normaliza (F (a-c) b )
                    | otherwise =  normaliza  (F ((a*d) - (b*c)) (b*d))
 (F a b) * (F c d) = (F (a*c) (b*d))

 abs (F a b) = (F (abs a) (abs b)) 
 negate (F a b) | a>0 && b > 0 = normaliza (F a (-b))
                | a>0 && b < 0 = normaliza (F a b)
                | a<0 && b > 0 = normaliza (F a b)
                | otherwise =  normaliza (F a (-b))
 signum (F a b) | a== 0 = 0 
                | a>0 && b > 0 = (1)
                | a>0 && b < 0 = (-1)
                | a<0 && b > 0 = (-1)
                | otherwise = (1)
 fromInteger x = F x 1




f2 :: [Frac]
f2 = [(F 1 2),(F 3 2),(F 4 2) ]


--f
meteoninoladentro :: Frac -> [Frac] -> [Frac]
meteoninoladentro _ [] = []
meteoninoladentro (F a b) (h:t) | (F (2*a) b) < h = h:(meteoninoladentro (F a b ) t)
                                |otherwise = meteoninoladentro  (F a b ) t  


--2
data Exp a =Const a
           |Simetrico (Exp a)
           |Mais (Exp a) (Exp a)
           |Menos (Exp a) (Exp a) 
           |Mult (Exp a) (Exp a)

--a
instance Show a => Show (Exp a) where
  show (Const a) = show a 
  show (Simetrico a) =  "(-" ++ (show a)  ++ ")"
  show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Menos a b) = "(" ++ show a ++" - " ++ show b ++ ")"
  show (Mult a b) = "("++  show a ++ " * " ++ show b ++ ")"


instance (Num a,Eq a) => Eq (Exp a) where
   a == b = (valor2 a) == (valor2 b) 


valor2 ::Num a => Exp a -> a
valor2 (Const a) = a
valor2 (Simetrico a) = - valor2 a
valor2 (Mais a b) = valor2 a + valor2 b 
valor2 (Menos a b) =  valor2 a - valor2 b
valor2 (Mult a b) = valor2 a * valor2 b

--c)

instance (Num a, Eq a) => Num (Exp a) where
    x + y = Const (valor2 x + valor2 y)
    --
    x - y = Const (valor2 x - valor2 y)
    --
    x * y = Const (valor2 x * valor2 y)
    --
    negate (Const a) = Const (- a)
    negate (Simetrico a) = a
    negate (Mais a b) = Mais (- a) (- b)
    negate (Menos a b) = Menos b a
    negate (Mult a b) = Mult (-a) b
    fromInteger x = Const (fromInteger x)
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs (Mais a b) = abs (a + b)
    abs (Menos a b) = abs (a - b)
    abs (Mult a b) = abs (a * b)
    signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
    signum (Simetrico a) = - signum a
    signum (Mais a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
    signum (Menos a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
    signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))



data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

--3