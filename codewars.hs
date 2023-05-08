--decomp' :: Int -> Int -> [Int]
--decomp' 0 _ = []
--decomp' 1 _ = []
--decomp' _ 0 = []
--decomp' _ 1 = []
--decomp' n x
--  | 2 * x > n = []
--  | mod n x == 0 = [x] ++ (decomp' (div n x) x)
--  | otherwise = decomp' n (x + 1)
import Data.List
import Data.Char

decomp' :: Int -> Int -> [Int]
decomp' n 1 = [] ++ (decomp' n 2)
decomp' 1 _ = []
decomp' n x
  | x > n = []
  | mod n x == 0 = [x] ++ (decomp' (div n x) x)
  | otherwise = decomp' n (x + 1)

decomp :: Int -> String
decomp n =  concat $ map (\x -> "(" ++ show (head x) ++ ")" ++ if (length x) > 1 then "**("++ show (length x) ++ ")" else "") ((group . sort) $ decomp' n 1)

rotate :: [[a]] -> [[a]]
rotate = (transpose . map reverse)

snail :: [[Int]] -> [Int]
snail [] = []
snail x = head x ++ snail ((transpose . map reverse) (drop 1 x))

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, mod x p /= 0]
--jointerval :: [[Int]] -> [Int]
--jointerval xs
--jointerval xs
--  | length xs == 1 = head xs
--  | otherwise = if ((head.head) xs) < (head (xs!!2)) && ((last.head) xs) > (head (xs!!2))  then [s ] ++ jointerval (drop 2 xs) else [head xs] ++ [xs!!2] ++ jointerval (drop 2 xs) 


toYear :: (Integral i, Show i) => i -> String
toYear n = (show years) ++ " year" ++ (if years > 1 then "s" else "")
  where years = div n 31536000

toDay :: (Integral i, Show i) => i -> String
toDay n = (show days) ++ " day" ++ (if days > 1 then "s" else "")
  where days = div n 86400

toHour :: (Integral i, Show i) => i -> String
toHour n = (show hours) ++ " hour" ++ (if hours > 1 then "s" else "")
  where hours = div n 3600

toMinute :: (Integral i, Show i) => i -> String
toMinute n = (show minutes) ++ " minute" ++ (if minutes > 1 then "s" else "")
  where minutes = div n 60

toSecond :: (Integral i, Show i) => i -> String
toSecond seconds = (show seconds) ++ " second" ++ (if seconds > 1 then "s" else "")


formatDuration' :: (Integral i, Show i) => i -> [String]
formatDuration' n
  | n >= 31536000 = [toYear n] ++ formatDuration'(mod n 31536000)
  | n >= 864000 = [toDay n] ++ formatDuration'(mod n 86400)
  | n >= 3600 = [toHour n] ++ formatDuration'(mod n 3600)
  | n >= 60 = [toMinute n] ++ formatDuration'(mod n 60)
  | n > 0  = [toSecond n]
  | otherwise  = []

addAnd :: [String] -> [String]
addAnd xxs
  | length xxs > 1 = take ((length xxs) - 2) xxs ++ [(last (init xxs)) ++ " and " ++ (last xxs)]
  | otherwise = xxs

formatDuration :: (Integral i, Show i) => i -> String
formatDuration 0 = "now"
formatDuration n = intercalate ", " (addAnd (formatDuration' n))

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' [a] = [[a]]
permutations' (x:xs) =  (>>=) (permutations' xs)  (merge x)
    where
        insertAt :: Int -> a-> [a] -> [a]
        insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs
        merge :: a -> [a] -> [[a]]
        merge x p = map (\i -> insertAt i x p) [0..length p]

top3 :: [Char] -> [[Char]]
top3 s = map (\(x,y)->y) $ take 3 (reverse $ sort [(length x', head x') | x' <- (group $ sort (words [toLower x| x <- s, elem (toLower x) ['a'..'z'] || x == '\'' || x == ' ']))])


extractRange :: [Int] -> String
extractRange = intercalate "," . f
  where f :: [Int] -> [String]
        f (x1 : x2 : x3 : xs) | x1 + 1 == x2 && x2 + 1 == x3
             = (show x1 ++ '-' : show xn) : f xs'
          where (xn, xs') = g (x3 + 1) xs
                g a (n : ns) | a == n    = g (a + 1) ns
                             | otherwise = (a - 1, n : ns)
                g a []                   = (a - 1, [])
        f (x : xs)            = show x : f xs
        f []                  = []

isGreen :: Int -> Bool
isGreen x = (mod (x^2 - x) (10^(floor(logBase 10 (fromIntegral x)) + 1))) == 0
