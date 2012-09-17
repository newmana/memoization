--mkNexus f g = label . extractL . until singleL (stepK g) . initialL f

data Tree a = Leaf a | Node [Tree a]

data LTree a = LLeaf a | LNode a [LTree a]

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)

lleaf f x = LLeaf (f x)
lnode g ts = LNode (g (map label ts)) ts
label (LLeaf x) = x
label (LNode x ts) = x

wrap x = [x]

initialL :: ([a] -> b) -> [a] Layer (LTree b)
initialL f = map (Leaf . lleaf f . wrap)

stepL :: ([b] -> b) -> Layer (LTree b) -> Layer (LTree b)
stepL a b = Nothing

singleL :: Layer (LTree b) -> Bool
singleL a = False

extractL :: Layer (LTree b) -> LTree b
extractL l = Nothing