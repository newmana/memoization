-- Based on "Pearls of Functional Algorithm Design", Chapter 21 "Hylomorphisms and Nexuses", pp 173-179
import Data.Time
import Data.List

countdown :: Int -> [Int] -> (Expr, Value)
countdown n = nearest n . concatMap mkExprs . subseqs

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

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = (v1 <= v2)
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = (1 < v1) && (v1 <= v2)
legal Div v1 v2 = (1 < v2) && (v1 `mod` v2 == 0)

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2] ++
        [(App op e2 e1, apply op v2 v1) | op <- ops, legal op v2 v1]
ops = [Add, Sub, Mul, Div]

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)

unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x) else
  Node (map (unfold p v h) (h x))

data LTree a = LLeaf a | LNode a [LTree a] deriving (Show, Eq)

mkExprs :: [Int] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs = [ev | (ys, zs) <- unmerges xs,
		ev1 <- mkExprs ys,
		ev2 <- mkExprs zs,
		ev <- combine ev1 ev2]
		
treeToNexus :: Tree [a] -> LTree [a]
treeToNexus = fill id recover

lnode :: ([a] -> a) -> [LTree a] -> LTree a
lnode g ts = LNode (g (map label ts)) ts

lleaf :: (t -> a) -> t -> LTree a
lleaf f x = LLeaf (f x)

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)

recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

unmerge :: ([a], [b]) -> [(a, b)]
unmerge (xs, ys) = (zip xs (reverse ys))

halved :: [LTree a] -> ([a], [a])
halved ts = halve (tail (traverse (forest 0 ts)))

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

label :: LTree t -> t
label (LLeaf x) = x
label (LNode x ts) = x

single :: [a] -> Bool
single x 
  | length x == 1 = True
  | otherwise = False

mkTree :: ([a] -> [[a]]) -> [a] -> Tree [a]
mkTree h = unfold single id h

minors :: [a] -> [[a]]
minors [x, y] = [[x], [y]]
minors (x : xs) = map (x :) (minors xs) ++ [xs]

unmerges :: [a] -> [([a], [a])]
unmerges x = unmerge $ halved $ [treeToNexus $ mkTree minors x]
	
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
