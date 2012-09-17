facs = scanl (*) 1 [1..]
fac n = facs !! n

fac = foldr (*) 1 . enumFromTo 1

