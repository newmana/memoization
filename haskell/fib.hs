fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Int]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]
zipFib n = fibs !! n