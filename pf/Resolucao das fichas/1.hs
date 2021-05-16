--21
apagaelementos :: Eq a => a -> [a] ->[a]
apagaelementos _ [] = []
apagaelementos x (h:xs) | (x==h) = xs
						|otherwise = [h]++ (apagaelementos x xs)


remove2 :: Eq a => [a]-> [a]-> [a]
remove2 l [] = l
remove2 [] _ = []
remove2 l (h1:hs) = remove2 (apagaelementos h1 l) hs

