module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just (collatzSteps n 0)

collatzSteps :: Integer -> Integer -> Integer
collatzSteps n acc
  | n == 1 = acc
  | odd n = collatzSteps (3*n+1) (acc + 1)
  | even n = collatzSteps (div n 2) (acc+1)
  | otherwise = acc
