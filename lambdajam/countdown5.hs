-- Based on "Pearls of Functional Algorithm Design", Chapter 21 "Hylomorphisms and Nexuses", pp 173-179
import Data.Time
import Data.List hiding (group)

countdown :: Int -> [Int] -> (Expr, Value)
--countdown n = nearest n . mkNexus id recover . subseqs
countdown = undefined

display f = do
	start <- getCurrentTime
	result <- print $ f
	stop <- getCurrentTime
	print $ diffUTCTime stop start
	return result

subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
        where xss = subseqs xs

data Op = Add | Sub | Mul | Div deriving (Show, Eq)
data Expr = Num Int | App Op Expr Expr deriving (Show, Eq)
type Value = Int

value :: Expr -> Value
value (Num x) = x
value (App op e1 e2) = apply op (value e1) (value e2)

apply :: Op -> Value -> Value -> Value
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = (div)

non :: Op -> Expr -> Bool
non op (Num x) = True
non op1 (App op2 e1 e2) = op1 /= op2

legal :: Op -> (Expr, Value) -> (Expr, Value) -> Bool
legal Add (e1, v1)(e2, v2) = (v1 <= v2) && non Sub e1 && non Add e2 && non Sub e2
legal Sub (e1, v1)(e2, v2) = (v2 < v1) && non Sub e1 && non Sub e2
legal Mul (e1, v1)(e2, v2) = (1 < v1 && v1 <= v2) && non Div e1 && non Mul e2 && non Div e2
legal Div (e1, v1)(e2, v2) = (1 < v2 && v1 `mod` v2 == 0) && non Div e1 && non Div e2

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1)(e2, v2)
  | v1 < v2  = comb1(e1, v1)(e2, v2)
  | v1 == v2 = comb2(e1, v1)(e2, v2)
  | v1 > v2  = comb1(e2, v2)(e1, v1)

comb1 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
comb1 (e1, v1)(e2, v2) =
  (if non Sub e1 && non Sub e2
    then [(App Add e1 e2, v1 + v2) | non Add e2] ++ [(App Sub e2 e1, v2 - v1)]
    else []) ++
  (if 1 < v1 && non Div e1 && non Div e2
    then [(App Mul e1 e2, v1 * v2) | non Mul e2] ++ [(App Div e2 e1, q) | r == 0]
    else [])
    where (q, r) = divMod v2 v1

comb2 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
comb2 (e1, v1)(e2, v2) =
  [(App Add e1 e2, v1 + v2) | non Sub e1, non Add e2, non Sub e2] ++
  (if 1 < v1 && non Div e1 && non Div e2
    then [(App Mul e1 e2, v1 * v2) | non Mul e2] ++ [(App Div e1 e2, 1)]
    else [])

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)

unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x) else
  Node (map (unfold p v h) (h x))

data LTree a = LLeaf a | LNode a [LTree a] deriving (Show, Eq)
data Layer a = Layer [a]

recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

mkNexus f g = label . extractL . until singleL (stepL g) . initialL f

initialL f = map (lleaf f . wrap)

singleL = single

--stepL g = map (mapTree (lnode g)) . group
stepL g = map (lnode g) . group

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node []) = Node ([])
mapTree f (Node (x:xs)) = joinTree (Node [mapTree f x]) (mapTree f (Node xs))

joinTree (Node t1) (Node t2) = Node (foldr (:) t1 t2)
joinTree l@(Leaf a) t = Node [l, t]
joinTree t l@(Leaf a) = Node [t, l]

--group :: [Tree a] -> [Tree [a]]
--group [t] = []
--group (Leaf x : vs) = Node [Leaf[x,y] | Leaf y <- vs] : group vs
--group (Node us : vs) = Node (zipWith combineG (group us) vs) : group vs
group [] = []
group (x:y:xs) = [x,y]:group xs

combineG (Leaf xs)(Leaf x) = Leaf (xs ++ [x])
combineG (Node us)(Node vs) = Node (zipWith combineG us vs) 

extractL = head
--extractL = extract . head
--  where 
--    extract (Leaf x) = x
--    extract (Node [t]) = extract t

wrap x = [x]

label :: LTree t -> t
label (LLeaf x) = x
label (LNode x ts) = x

lnode g ts = LNode (g (zip xs)(reverse ys)) ts
  where (xs, ys) = halve(traverse(forest 0 ts))

halve :: [a] -> ([a], [a])
halve = foldr (\x (ys, zs) -> (x : zs, ys)) ([], [])

forest :: Int -> [LTree a] -> [LTree a]  
forest k [] = []
forest k (LLeaf x : ts) = LLeaf x : ts
forest k (LNode x us : vs) = LNode x (forest k (drop k us)) : forest (k + 1) vs

traverse :: [LTree a] -> [a]
traverse [] = []
traverse ts = map label ts ++ traverse (concatMap subtrees ts)

subtrees :: LTree t -> [LTree t]
subtrees (LLeaf x) = []
subtrees (LNode x ts) = ts

single :: [a] -> Bool
single x 
  | length x == 1 = True
  | otherwise = False

lleaf :: (t -> a) -> t -> LTree a
lleaf f x = LLeaf (f x)

minors :: [a] -> [[a]]
minors [x, y] = [[x], [y]]
minors (x : xs) = map (x :) (minors xs) ++ [xs]

unmerges :: [a] -> [([a], [a])]
unmerges x = undefined
--unmerges x = unmerge $ halved $ [treeToNexus $ mkTree minors x]
	
split xs = [take n xs, drop n xs] 
  where n = l `div` 2
        l = length xs

isegs xs = [init xs, tail xs]

nearest n ((e,v) : evs) = if d == 0 then (e,v)
	else search n d (e, v) evs
	where d = abs (n - v)
	
search n d ev [] = ev
search n d ev ((e, v) : evs)
	| d' == 0 = (e,v)
	| d' < d = search n d' (e,v) evs
	| d' >= d = search n d ev evs
		where d' = abs (n - v)
		
main = do
    display (countdown 831 [1,3,7,10,25,50])
    display (countdown 12830 [1,3,7,11,21,51])
    display (countdown 53281 [1,3,5,7,9,12,50])
    display (countdown 58101 [5,6,7,8,9,10,11,12])      
