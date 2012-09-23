module Timer 
  (displayTime) where

import Data.Time

displayTime f = do
	start <- getCurrentTime
	result <- print $ f
	stop <- getCurrentTime
	print $ diffUTCTime stop start
	return result
