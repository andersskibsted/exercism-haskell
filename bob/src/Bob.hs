module Bob (responseFor) where
import Data.Char (toUpper, isAlpha, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilent xs = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion $ dropWhileEnd isSpace xs = "Sure."
  | isYelling xs = "Whoa, chill out!"
  | otherwise = "Whatever."




isQuestion :: String -> Bool
isQuestion [x] = x == '?'
isQuestion (_:xs) = isQuestion xs
isQuestion _ = False


isYelling :: String -> Bool
isYelling str = any isAlpha str && filter isAlpha str == map toUpper (filter isAlpha str)

isSilent :: String -> Bool
-- isSilent str = all (== ' ') str || all (== '\n') str || all (== '\t') str || all (== '\t') str
isSilent = all isSpace
