data LeafTree x = Leaf x | Split (LeafTree x) (LeafTree x)

-- Simple catamorphism - T -> U - recursively through T, destroying it to give U
treeSum = Leaf x = x
treeSum (Split l r) = treeSum l + treeSum r

