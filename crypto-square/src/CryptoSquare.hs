module CryptoSquare (encode) where
import Data.Char (toLower, isAlpha, isAlphaNum, isSpace)
import GHC.Real (fromIntegral)
import Data.List (dropWhileEnd)

encode :: String -> String
encode [] = []
encode xs
  | mesLength < 4 = normalized
  | mesLength <= r*c = init $ concatMap (flip (++) " " . padToLength r)
                       ([ [normalized !! (i*c+n) | i <- [0..r-1], i*c+n < mesLength] | n <- [0..c-1] ])
  | otherwise = init $ concatMap (flip (++) " " . padToLength c)
                ([ [normalized !! (i*c+n) | i <- [0..c-1], i*c+n < mesLength] | n <- [0..c-1] ])

        where mesLength = length normalized
              normalized = filter isAlphaNum $ map toLower xs
              r = floor $ sqrt $ fromIntegral mesLength
              c = ceiling $ sqrt $ fromIntegral mesLength


padToLength :: Int -> String -> String
padToLength len [] = padToLength len " "
padToLength len str
  | length str == len = str
  | length str > len = str
  | otherwise = padToLength len (str ++ " ")

