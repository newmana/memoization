import Data.Time
import Timer

countdown :: Int -> [Int] -> (Expr, Value)
countdown n = nearest n . concatMap toExprs . extract . memoise . subseqs

subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
	where xss = subseqs xs

data Op = Add | Sub | Mul | Div deriving (Show)
data Expr = Num Int | App Op Expr Expr deriving (Show)
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

legal2 :: Op -> Value -> Value -> Bool
legal2 Add v1 v2 = (v1 <= v2)
legal2 Sub v1 v2 = (v2 < v1)
legal2 Mul v1 v2 = (1 < v1) && (v1 <= v2)
legal2 Div v1 v2 = (1 < v2) && (v1 `mod` v2 == 0)

empty :: Memo
empty = Node[][]

fetch :: Memo -> [Int] -> [Tree]
fetch (Node es xms)[] = es
fetch (Node es xms)(x:xs) = fetch(follow x xms) xs

store :: [Int] -> [Tree] -> Memo -> Memo
store [x] es (Node fs xms) = Node fs ((x, Node es[]) : xms)
store (x:xs) es (Node fs xms)
  = Node fs (yms ++ (x, store xs es m) : zms)
    where (yms, (z, m) : zms) = break (equals x) xms
	  equals x (z, m) = (x == z)
	  
follow :: Int -> [(Int, Memo)] -> Memo
follow x xms = head [m | (x', m) <- xms, x == x']

extract :: Memo -> [Tree]
extract (Node es xms) = es ++ concatMap (extract . snd) xms

memoise :: [[Int]] -> Memo
memoise = foldl insert empty
insert memo xs = store xs (mkTrees memo xs) memo

data Tree = Tip Int | Bin Tree Tree
data Trie a = Node a [(Int, Trie a)]
type Memo = Trie [Tree]

mkTrees :: Memo -> [Int] -> [Tree]
mkTrees memo [x] = [Tip x]
mkTrees memo xs = [Bin t1 t2 | (ys, zs) <- unmerges xs,
  t1 <- fetch memo ys,
  t2 <- fetch memo zs]
  
toExprs :: Tree -> [(Expr, Value)]
toExprs (Tip x) = [(Num x, x)]
toExprs (Bin t1 t2) = [ev | ev1 <- toExprs t1, ev2 <- toExprs t2,
  ev <- combine ev1 ev2] 
		
unmerges :: [a] -> [([a], [a])]
unmerges [x, y] = [([x], [y])]
unmerges (x:xs) = [([x], xs)] ++
	concatMap (add x) (unmerges xs)
	where add x (ys, zs) = [(x : ys, zs), (ys, x : zs)]
	
combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal2 op v1 v2] ++
	[(App op e2 e1, apply op v2 v1) | op <- ops, legal2 op v2 v1]
ops = [Add, Sub, Mul, Div]

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
  displayTime (countdown 12831 [1,3,7,10,11,12,14,50])
  displayTime (countdown 53280 [2,3,7,10,12,19,24,50])
  displayTime (countdown 53280 [2,5,8,10,11,17,24,50])