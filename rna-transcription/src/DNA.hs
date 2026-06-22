module DNA (toRNA) where

toRNA :: String -> Either Char String
-- toRNA [x] = Left (rnaMapping x)
toRNA [] = Right ""
toRNA xs = if all charDNA xs
  then Right (map rnaMapping xs)
  else Left (head (filter (not . charDNA) xs))

charDNA :: Char -> Bool
charDNA c = c == 'G' || c == 'C' || c == 'T' || c == 'A'

rnaMapping :: Char -> Char
rnaMapping 'G' = 'C'
rnaMapping 'C' = 'G'
rnaMapping 'T' = 'A'
rnaMapping 'A' = 'U'
rnaMapping x = x
