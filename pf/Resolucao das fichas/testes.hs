module Testes where

--exame 18-19

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])


convLP :: RelL a -> RelP aconvLP l = concat (map junta l)
                                 where junta (x,xs) = map (\y->(x,y)) xs


rp1 :: :: RelP Int
rp1= [(1,3),(1,4),(2,1),(2,4),(2,5),(3,7),(4,7),(5,7),(6,5),(7,6)]

--a)
convPL ::  (Eq a) => RelP a -> RelL
convPL [] = []
convPL ((x,y):t) = Let (lx,fx)= filtra x l 
                   in (x,y:lx):convPL fx 




filtra :: (Eq a) =>a ->RelP a ->([a],RelP a)
filtra x [] = ([],[])
filtra x ((y,z):t) Let (lx,fx) =filtra x l 
                   in if x == y 
                      then (z:(x,fx))
                      else (lx,(y,z)):


--b)

criaRelPint ::  Int -> IO (RelP Int)
criaRelPint 0 = return []
criaRelPint n = do putStr "Insira um par de inteiros"
                    s<-getline
                    l <- criaRelPint (n-1)
                    return ((read s):l)


--c)

convFP ::  (Eq a) => RelF a -> RelP a
convFP (l,g) =convLP ( map(\x->(x,g x) l))

--d


