data LeafTree x = Leaf x | Split (LeafTree x) (LeafTree x) deriving (Eq, Show)
a = Split (Leaf 2) (Leaf 3)

-- Simple catamorphism - T -> U - recursively through T, destroying it to give U
treeSum (Leaf x) = x
treeSum (Split l r) = treeSum l + treeSum r

type LeafTreeCata x u = (x -> u, u -> u -> u)

leafTreeCata :: LeafTreeCata x u -> LeafTree x -> u
leafTreeCata (fl, fs) = cata where
	cata (Leaf x) = fl x
	cata (Split l r) = fs (cata l) (cata r)
	
treeSum2 = leafTreeCata (id, (+))

fibTree n 
  | n < 2 = Leaf 1
  | otherwise = Split (fibTree (n - 1)) (fibTree (n - 2))
  
type LeafTreeAna u x = u -> Either x (u, u)

leafTreeAna :: LeafTreeAna u x -> u -> LeafTree x
leafTreeAna d = ana where
	ana t = case d t of
		Left l -> Leaf l
		Right (l, r) -> Split (ana l) (ana r)
		
fibTree2 = leafTreeAna destructFib
destructFib n 
  | n < 2 = Left 1
  | otherwise = Right(n - 1, n - 2)