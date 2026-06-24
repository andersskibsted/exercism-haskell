module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | isDNA xs = Right $ fromList [countAs xs, countCs xs, countGs xs, countTs xs]
  | otherwise = Left "Not dna"
    where isDNA = all (`elem` "ACGT")


countAs :: String -> (Nucleotide, Int)
countAs str = (A, length (filter (== 'A') str))

countCs :: String -> (Nucleotide, Int)
countCs str = (C, length (filter (== 'C') str))

countGs :: String -> (Nucleotide, Int)
countGs str = (G, length (filter (== 'G') str))

countTs :: String -> (Nucleotide, Int)
countTs str = (T, length (filter (== 'T') str))
