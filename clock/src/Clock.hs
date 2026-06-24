module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  let extraH = div min 60 in
  let h = mod (extraH + hour) 24 in
  Clock h (mod min 60)

toString :: Clock -> String
toString (Clock h m)
  | h < 10 && m < 10 = "0" ++ show h ++ ":0" ++ show m
  | h < 10 = "0" ++ show h ++ ":" ++ show m
  | m < 10 = show h ++ ":0" ++ show m
  | otherwise = show h ++ ":" ++ show m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = fromHourMin (hour+h) (min+m)
