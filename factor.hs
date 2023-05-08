factor :: Integer -> Integer -> [Integer]
factor 0 _ = []
factor 1 _ = []
factor n m
  | (mod n m) == 0 = [m] ++ factor (div n m) (m)
  | otherwise = factor n (m - 1)
