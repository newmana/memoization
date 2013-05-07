-- Based on "Pearls of Functional Algorithm Design", Chapter 21 "Hylomorphisms and Nexuses", pp 173-179
import Data.Time
import Data.List hiding (insert)

countdown :: Int -> [Int] -> (Expr, Value)
countdown n = nearest n . extract . memoise . subseqs

display f = do
        start <- getCurrentTime
        result <- print $ f
        stop <- getCurrentTime
        print $ diffUTCTime stop start
        return result

subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ ([x] : map (x:) xss)
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

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = (v1 <= v2)
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = (1 < v1) && (v1 <= v2)
legal Div v1 v2 = (1 < v2) && (v1 `mod` v2 == 0)

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2] ++
        [(App op e2 e1, apply op v2 v1) | op <- ops, legal op v2 v1]
ops = [Add, Sub, Mul, Div]

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
extract (Node es xms) = es ++ (concatMap (extract . snd) xms)

memoise :: [[Int]] -> Memo
memoise = foldl' insert empty
insert memo xs = store xs (mkExprs memo xs) memo

data Trie a = Node a [(Int, Trie a)] deriving (Show)
type Memo = Trie [(Expr, Value)]
		
unmerges :: [a] -> [([a], [a])]
unmerges [x, y] = [([x], [y])]
unmerges (x:xs) = [([x], xs)] ++ (concatMap (add x) (unmerges xs))
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
    display (countdown 831 [1,3,7,10,25,50])
    display (countdown 12830 [1,3,7,11,21,51])
    display (countdown 53281 [1,3,5,7,9,12,50])
    display (countdown 58101 [5,6,7,8,9,10,11,12])
