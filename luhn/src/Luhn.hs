module Luhn (isValid) where
import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid [_] = False
isValid n
  | len < 2 = False
  | otherwise = mod (checkLuhn normalized) 10 == 0
        where len = length normalized
              normalized = filter isDigit n

checkLuhn :: String -> Int
checkLuhn [] = 0
checkLuhn [x] = digitToInt x
checkLuhn (x:y:xs)
  | even $ length (x:y:xs) = doubleDigit (digitToInt x) + digitToInt y + checkLuhn xs
  | otherwise = doubleDigit (digitToInt y) + digitToInt x + checkLuhn xs

doubleDigit :: Int -> Int
doubleDigit n
  | dn > 9 = dn - 9
  | otherwise = dn
        where dn = 2*n
