mkNexus f g = label . extractL . until singleL(stepL g) . initialL f

data Tree a = Leaf a | Node [Tree a]
data LTree a = LLeaf a | LNode a [LTree a]
data Layer a = Layer (LTree a)

--fold :: a -> b -> ([b] -> b) -> Tree a -> b
--fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)

unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x) else
	Node (map (unfold p v h) (h x))

lleaf f x = LLeaf (f x)
lnode g ts = LNode (g (map label ts)) ts
label (LLeaf x) = x
label (LNode x ts) = x
wrap x = [x]

--hylo = label . fill f g . unfold p id h
--fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
--fill f g = fold (lleaf f) (lnode g)

initialL :: ([a] -> b) -> [a] -> Layer (LTree b)
initialL f = map (Leaf . lleaf f . wrap)

singleL :: Layer (LTree b) -> Bool
singleL a = False

extractL :: Layer (LTree b) -> LTree b
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
