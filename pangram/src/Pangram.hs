module Pangram (isPangram) where
import Data.Char (toLower, toUpper)

isPangram :: String -> Bool
isPangram text = isInTheOther (map toUpper text) (map toUpper "abcdefghijklmnopqrstuvwxyz")


isInTheOther :: String -> String -> Bool
isInTheOther [] [] = True
isInTheOther [] alph = False
isInTheOther str [] = True
isInTheOther (x:xs) alph =
  isInTheOther xs (filter (/= x) alph)
