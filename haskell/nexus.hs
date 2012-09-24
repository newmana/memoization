-- Based on "Pearls of Functional Algorithm Design", Chapter 21 "Hylomorphisms and Nexuses", pp 172-179

data Tree a = Leaf a | Node [Tree a]

fold :: a -> b -> ([b] -> b) -> Tree a -> b
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

initialL :: ([a] -> b) -> [a] -> Layer (LTree b)
initialL f = map (Leaf . lleaf f . wrap)

singleL :: Layer (LTree b) -> Bool
singleL a = single

--extractL :: Layer (LTree b) -> LTree b
extractL l = extract . head
  where extract (Leaf x) = x
        extract (Node [t]) = extract t

stepL :: ([b] -> b) -> Layer (LTree b) -> Layer (LTree b)
stepL g = map (mapTree (lnode g)) . group

group :: [Tree a] -> [Tree [a]]
group [t] = []
group (Leaf x : vs) = Node [Leaf [x, y] | Leaf y <- vs] : group vs
group (Node us : vs) = Node (zipWith combine (group us) vs) : group vs

mapTree f (Leaf a)= Leaf (f a) 
mapTree f (Node xl xr ) = Node (mapTree f xl) (mapTree f xr)

combine (Leaf xs) (Leaf x) = Leaf (xs ++ [x])
combine (Node us) (Node vs) = Node (zipWith combine (group us) vs) : group vs

mkNexus f g = label . extractL . until singleL(stepL g) . initialL f