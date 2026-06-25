module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just ([ i | i <- [2..], isItPrime i ] !! (n-1))


isItPrime :: Integer -> Bool
isItPrime 1 = False
isItPrime n = and [ n `mod` i /= 0 | i <- [2..squareRoot] ]
  where squareRoot = floor $ sqrt (fromIntegral n)
