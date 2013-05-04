-- Based on "Pearls of Functional Algorithm Design", Chapter 21 "Hylomorphisms and Nexuses", pp 173-179
import Data.Time

countdown :: Int -> [Int] -> (Expr, Value)
countdown n = nearest n . extract . memoise . subseqs

display f = do
        start <- getCurrentTime
        result <- print $ f
        stop <- getCurrentTime
        print $ diffUTCTime stop start
        return result

subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
	where xss = subseqs xs

data Op = Add | Sub | Mul | Div deriving (Eq, Show)
data Expr = Num Int | App Op Expr Expr deriving (Eq, Show)
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

mkExprs :: Memo -> [Int] -> [(Expr, Value)]
mkExprs memo [x] = [(Num x, x)]
mkExprs memo xs = [ev | (ys, zs) <- unmerges xs,
		ev1 <- fetch memo ys,
		ev2 <- fetch memo zs,
		ev <- combine ev1 ev2]
		
empty :: Memo
empty = Node[][]

fetch :: Memo -> [Int] -> [(Expr, Value)]
fetch (Node es xms)[] = es
fetch (Node es xms)(x:xs) = fetch(follow x xms) xs

store :: [Int] -> [(Expr, Value)] -> Memo -> Memo
store [x] es (Node fs xms) = Node fs ((x, Node es[]) : xms)
store (x:xs) es (Node fs xms)
  = Node fs (yms ++ (x, store xs es m) : zms)
    where (yms, (z, m) : zms) = break (equals x) xms
	  equals x (z, m) = (x == z)
	  
follow :: Int -> [(Int, Memo)] -> Memo
follow x xms = head [m | (x', m) <- xms, x == x']

extract :: Memo -> [(Expr, Value)]
extract (Node es xms) = es ++ concatMap (extract . snd) xms

memoise :: [[Int]] -> Memo
memoise = foldl insert empty
insert memo xs = store xs (mkExprs memo xs) memo

data Trie a = Node a [(Int, Trie a)] deriving (Show)
type Memo = Trie [(Expr, Value)]
		
unmerges :: [a] -> [([a], [a])]
unmerges [x, y] = [([x], [y])]
unmerges (x:xs) = [([x], xs)] ++
	concatMap (add x) (unmerges xs)
	where add x (ys, zs) = [(x : ys, zs), (ys, x : zs)]
	
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
    display (countdown 128310 [1,3,7,10,11,12,14,50])
    display (countdown 532800 [2,3,7,10,12,19,24,50])
    display (countdown 532800 [2,5,8,10,11,17,24,50])	
