int2string :: Int -> String
int2string n = show n

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

any' :: (Eq a, Num a) => a -> [a] -> Bool
any' _ [] = False
any' x (y:ys)
  | x == y = True
  | otherwise = any' x ys


sumseven :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
sumseven a b c d e f g = a + b + c + d + e + f + g
