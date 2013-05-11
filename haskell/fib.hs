import Data.MemoTrie

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
fib' n = fib'' n
  where
    mfib = memo fib'
    fib'' 0 = 0
    fib'' 1 = 1
    fib'' n = mfib (n - 1) + mfib (n - 2)

main = do
  print $ fib'(40)
