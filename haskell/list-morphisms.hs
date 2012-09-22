-- Simple catamorphism - T -> U - recursively through T, destroying it to give U
prod [] = 1
prod (x:xs) = x * prod xs

-- Catamorphism - Reduces a list
type ListCata x u = (u, x -> u -> u)
listCata :: ListCata x u -> [x] -> u
listCata (a, f) = cata where
	cata [] = a
	cata (x:xs) = f x (cata xs)

-- The same prod
prod2 = listCata (1, (*))
rev = listCata ([], (\a b -> b ++ [a]))

-- Simple anamorphism - U -> T - recursively through U, destroying it to give T
count 0 = []
count n  = n : count (n - 1)

type ListAna u x = u -> Either () (x, u)
listAna :: ListAna u x -> u -> [x]
listAna a = ana where
	ana u = case a u of
		Left _ -> []
		Right (x, xs) -> x : ana xs
		
count2 = listAna destructCount where
	destructCount 0 = Left ()
	destructCount n = Right (n, n - 1)

-- Given a way to create and destroy a data structure we can compose
-- two operations into 1 - this is called hylomorphism	
listHylo (a, c) = listCata a . listAna c
-- Factorial
fac = prod2 . count2