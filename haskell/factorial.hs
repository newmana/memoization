import Timer
import Data.Time

fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac n = n * fac (n - 1)

facs :: [Int]
facs = scanl (*) 1 [1..]
ffac n = facs !! n

main = do
  displayTime (fac 10)
  displayTime (ffac 10)
  displayTime (fac 20)
  displayTime (ffac 20)