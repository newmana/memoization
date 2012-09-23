import qualified Data.Monoid as M

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)

a = (Node [Node [Leaf [3],Leaf [4]],Node [Leaf [4],Leaf [5]]])
okFold1 = fold (-) (head) (Leaf 1) 5 == (-4)
okFold2 = fold id concat a == [3,4,4,5]

unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x) else
  Node (map (unfold p v h) (h x))

data LTree a = LLeaf a | LNode a [LTree a] deriving (Show, Eq)

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)

hylo :: ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo f g h = fold f g . mkTree h

type Layer a = [Tree a]

lleaf :: (t -> a) -> t -> LTree a
lleaf f x = LLeaf (f x)

lnode :: ([a] -> a) -> [LTree a] -> LTree a
lnode g ts = LNode (g (map label ts)) ts
-- lnode g ts = LNode zr ts
--   where 
-- 	zr = g (zip xs) (reverse ys)
-- 	(xs, ys) = halved xs ys

halved :: [LTree a] -> ([a], [a])
halved ts = halve (traverse (forest 0 ts))
halve xs = splitAt (div (length xs) 2) xs
  
forest k (LLeaf x : ts) = LLeaf x : ts
forest k (LNode x us : vs) = LNode x (forest k (drop k us)) : forest (k + 1) vs

traverse :: [LTree a] -> [a]
traverse [] = []
traverse ts = map label ts ++ traverse (concatMap subtrees ts)

subtrees (LLeaf x) = []
subtrees (LNode x ts) = ts

label :: LTree t -> t
label (LLeaf x) = x
label (LNode x ts) = x

wrap :: t -> [t]
wrap x = [x]

mkTree :: ([a] -> [[a]]) -> [a] -> Tree [a]
mkTree h = unfold single id h

single :: [a] -> Bool
single x 
  | length x == 1 = True
  | otherwise = False

okSingle1 = single [1] == True
okSingle2 = single [] == False
okSingle3 = single [1,2,3,4,5] == False

split :: [a] -> [[a]]
split xs = [take n xs, drop n xs]
  where n = div (length xs) 2

okSplit1 = split "abc" == ["a", "bc"]  
okSplit2 = split "abcd" == ["ab", "cd"]  

isegs :: [a] -> [[a]]
isegs xs = [init xs, tail xs]

okIsegs = isegs "abc" == ["ab", "bc"]

minors :: [a] -> [[a]]
minors [x, y] = [[x], [y]]
minors (x : xs) = map (x :) (minors xs) ++ [xs]

okMinors = (minors "abc" == ["ab", "ac", "bc"])

recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

okRI = (recover (isegs "abcd") == (id "abcd"))

allTests = [okSingle1, okSingle2, okSingle3, okSplit1, okSplit2, okIsegs, okMinors]
testAll = (M.getAll $ M.mconcat $ map M.All allTests) == True