data LeafTree a = Leaf | Split a (LeafTree a) (LeafTree a) deriving (Show, Eq)

--leaf x = Split x Leaf Leaf

insert x Leaf = Split x Leaf Leaf 
insert x (Split y l r)
    | x < y = Split y (insert x l) r
    | x > y = Split y l (insert x r)
    | otherwise = Split x l r 
