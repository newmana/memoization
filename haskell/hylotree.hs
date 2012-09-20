data Tree a = Leaf a | Node [Tree a]

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)

unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x) else
	Node (map (unfold p v h) (h x))

data LTree a = LLeaf a | LNode a [LTree a]

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)

hylo :: ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo f g h = fold f g . mkTree h

type Layer a = [Tree a]

lleaf f x = LLeaf (f x)
lnode g ts = LNode (g (map label ts)) ts
label (LLeaf x) = x
label (LNode x ts) = x
wrap x = [x]

mkTree h = unfold single id h

single x 
  | length x == 1 = True
  | otherwise = False

split xs = [take n xs, drop n xs]
  where n = div (length xs) 2
  
isegs xs = [init xs, tail xs]

recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss
