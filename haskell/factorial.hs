import Data.Time

display f = do
	start <- getCurrentTime
	result <- print $ f
	stop <- getCurrentTime
	print $ diffUTCTime stop start
	return result

fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac n = n * fac (n - 1)

facs :: [Int]
facs = scanl (*) 1 [1..]
ffac n = facs !! n

main = do
  display (fac 10)
  display (ffac 10)
  display (fac 20)
  display (ffac 20)