module Acronym (abbreviate) where
import Data.Char (isAlpha, toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate (x:xs) =
  reverse $ pickOutAcronym xs [x]

pickOutAcronym :: String -> String -> String
pickOutAcronym [] acc = acc
pickOutAcronym [x] acc = acc
pickOutAcronym (x:y:ys) acc
  | isSeparator' x && isAlpha y = pickOutAcronym ys $ toUpper y : acc
  | isLower x && isUpper y = pickOutAcronym ys $ toUpper y : acc
  | isUpper x && isLower y = pickOutAcronym ys $ toUpper x : acc
  | otherwise = pickOutAcronym (y:ys) acc
    where isSeparator' c = c == ' ' || c == '-'
