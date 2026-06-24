module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum (foldr (\l acc  -> if l `elem` acc then acc else l:acc) [] $ concat [takeWhile (< limit) [x, x*2..] | x <- filter (/= 0) factors])
