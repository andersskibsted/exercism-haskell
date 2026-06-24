module Anagram (anagramsFor) where
import Data.Char (toUpper, toLower)


anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\wrd -> stringToLower wrd /= stringToLower xs)
                              $ filter (\wrd -> anagramOf (stringToLower wrd) (stringToLower xs)) xss

stringToLower :: String -> String
stringToLower = map toLower

anagramOf :: String -> String -> Bool
anagramOf [] [] = True
anagramOf [] wrd = False
anagramOf cand [] = False
anagramOf (x:xs) (y:ys) = x `elem` (y:ys) && anagramOf xs (removeFirstInstance x (y:ys) [])

removeFirstInstance :: Char -> String -> String -> String
removeFirstInstance c [] acc = reverse acc
removeFirstInstance c (x:xs) acc
  | c == x = reverse acc ++ xs
  | c /= x = removeFirstInstance c xs (x:acc)
